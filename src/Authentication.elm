module Authentication exposing (Authentication, login, registerGuest)

import ApiUtils exposing (apiRequest, clientEndpoint)
import Http
import Json.Decode as JD
import Json.Encode as JE
import Task exposing (Task)
import Url.Builder


{-| This module supports logging in using existing accounts on arbitrary
Matrix homeservers using the Client-Server API.
-}
type Authentication
    = Unauthenticated
    | Guest AuthData
    | User AuthData


type alias AuthData =
    { homeserverUrl : String
    , userId : String
    , accessToken : String
    }


{-| Decodes "user\_id" and "access\_token" fields into an AuthData record.
Can be used for both /register and /login.
-}
decodeAuthData : String -> JD.Decoder AuthData
decodeAuthData homeserverUrl =
    JD.map2 (AuthData homeserverUrl)
        (JD.field "user_id" JD.string)
        (JD.field "access_token" JD.string)


registerGuest : String -> Task Http.Error AuthData
registerGuest homeserverUrl =
    apiRequest
        { method = "POST"
        , url = clientEndpoint homeserverUrl [ "register" ] [ Url.Builder.string "kind" "guest" ]
        , responseDecoder = decodeAuthData homeserverUrl
        , accessToken = Nothing
        , body = Http.stringBody "application/json" "{}"
        }


{-| Login by sending a POST request to the /login endpoint
Login type "m.login.password" and identifier type "m.id.user"
-}
login : { homeserverUrl : String, user : String, password : String } -> Task Http.Error AuthData
login { homeserverUrl, user, password } =
    apiRequest
        { method = "POST"
        , url = clientEndpoint homeserverUrl [ "login" ] []
        , responseDecoder = decodeAuthData homeserverUrl
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
