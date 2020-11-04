module Session exposing
    ( Kind(..)
    , Session
    , authenticatedRequest
    , incrementTransactionId
    , isUser
    , login
    , registerGuest
    , sessionKind
    , sessionStatusString
    , transactionId
    )

import ApiUtils exposing (clientEndpoint)
import Http
import Json.Decode as JD
import Json.Encode as JE
import Task exposing (Task)
import Url.Builder exposing (QueryParameter)



{- This module contains the UI and API interactions for logging in using temporary guest accounts or
   existing accounts on arbitrary Matrix homeservers using the Client-Server API.
-}


type Session
    = Session SessionData


type alias SessionData =
    { homeserverUrl : String
    , kind : Kind
    , txnId : Int
    , userId : String
    , accessToken : String
    }


{-| Decodes "user\_id" and "access\_token" fields into a Session record.
Can be used for both /register and /login.
-}
decodeSession : String -> Kind -> JD.Decoder Session
decodeSession homeserverUrl kind =
    JD.map2 (SessionData homeserverUrl kind 0)
        (JD.field "user_id" JD.string)
        (JD.field "access_token" JD.string)
        |> JD.map Session


{-| A natural language string that summarizes the auth status.
-}
sessionStatusString : Session -> String
sessionStatusString (Session session) =
    "Signed in as " ++ toString session.kind ++ " " ++ session.userId



-- TRANSACTION ID


{-| Increment the transaction id of this session.
Should be incremented on each message sent.
-}
incrementTransactionId : Session -> Session
incrementTransactionId (Session session) =
    Session { session | txnId = session.txnId + 1 }


{-| Get the current transaction Id
-}
transactionId : Session -> Int
transactionId (Session session) =
    session.txnId



-- KIND


type Kind
    = Guest
    | User


sessionKind : Session -> Kind
sessionKind (Session session) =
    session.kind


isUser : Session -> Bool
isUser (Session session) =
    session.kind == User


toString : Kind -> String
toString authType =
    case authType of
        Guest ->
            "guest"

        User ->
            "user"



-- API CALLS


{-| Make authenticated requests using a Session object.
Wraps ApiUtils.apiRequest
-}
authenticatedRequest :
    Session
    ->
        { method : String
        , path : List String
        , params : List QueryParameter
        , body : Http.Body
        , responseDecoder : JD.Decoder a
        }
    -> Task Http.Error a
authenticatedRequest (Session session) { method, path, params, body, responseDecoder } =
    apiRequest
        { method = method
        , url = clientEndpoint session.homeserverUrl path params
        , body = body
        , accessToken = Just session.accessToken
        , responseDecoder = responseDecoder
        }


{-| Make unauthenticated requests to a Matrix API
-}
unauthenticatedRequest : { method : String, url : String, body : Http.Body, responseDecoder : JD.Decoder a } -> Task Http.Error a
unauthenticatedRequest { method, url, body, responseDecoder } =
    apiRequest
        { method = method
        , url = url
        , body = body
        , responseDecoder = responseDecoder
        , accessToken = Nothing
        }


{-| Make an optionally authenticated requests to a Matrix homeserver.
-}
apiRequest :
    { method : String
    , url : String
    , accessToken : Maybe String
    , responseDecoder : JD.Decoder a
    , body : Http.Body
    }
    -> Task Http.Error a
apiRequest { method, url, accessToken, responseDecoder, body } =
    Http.task
        { method = method
        , headers =
            accessToken
                |> Maybe.map (\at -> [ Http.header "Authorization" <| "Bearer " ++ at ])
                |> Maybe.withDefault []
        , url = url
        , body = body
        , resolver = Http.stringResolver <| handleJsonResponse responseDecoder
        , timeout = Nothing
        }


{-| handle the JSON response of a HTTP Request
Flatten HTTP and JSON errors.
-}
handleJsonResponse : JD.Decoder a -> Http.Response String -> Result Http.Error a
handleJsonResponse decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.BadStatus_ { statusCode } _ ->
            Err (Http.BadStatus statusCode)

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.GoodStatus_ _ body ->
            case JD.decodeString decoder body of
                Err _ ->
                    Err (Http.BadBody body)

                Ok result ->
                    Ok result



-- AUTHENTICATION


{-| Register a guest account with the homeserver, by sending a HTTP POST to
/register?kind=guest. This presumes that the homeserver has enabled guest registrations.
-}
registerGuest : String -> Task Http.Error Session
registerGuest homeserverUrl =
    apiRequest
        { method = "POST"
        , url = clientEndpoint homeserverUrl [ "register" ] [ Url.Builder.string "kind" "guest" ]
        , responseDecoder = decodeSession homeserverUrl Guest
        , accessToken = Nothing
        , body = Http.stringBody "application/json" "{}"
        }


{-| Login by sending a POST request to the /login endpoint
Login type "m.login.password" and identifier type "m.id.user"
-}
login : { homeserverUrl : String, user : String, password : String } -> Task Http.Error Session
login { homeserverUrl, user, password } =
    apiRequest
        { method = "POST"
        , url = clientEndpoint homeserverUrl [ "login" ] []
        , responseDecoder = decodeSession homeserverUrl User
        , accessToken = Nothing
        , body = Http.jsonBody <| passwordLoginJson { user = user, password = password }
        }


{-| Make JSON Value for logging in using /login
Login type "m.login.password" and identifier type "m.id.user"
As described in the Client Server spec:
<https://matrix.org/docs/spec/client_server/r0.6.1#post-matrix-client-r0-login>
-}
passwordLoginJson : { user : String, password : String } -> JE.Value
passwordLoginJson { user, password } =
    JE.object
        [ ( "type", JE.string "m.login.password" )
        , ( "identifier"
          , JE.object
                [ ( "type", JE.string "m.id.user" )
                , ( "user", JE.string user )
                ]
          )
        , ( "password", JE.string password )
        , ( "initial_device_display_name", JE.string "Cactus Comments" )
        ]
