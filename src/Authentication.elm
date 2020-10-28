module Authentication exposing (Authentication, LoginForm, initLoginForm, login, registerGuest, viewLoginButton, viewLoginForm)

import Accessibility as Html exposing (..)
import ApiUtils exposing (apiRequest, clientEndpoint)
import Html.Attributes exposing (class, placeholder, type_)
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD
import Json.Encode as JE
import Task exposing (Task)
import Url.Builder



{- This module contains the UI and API interactions for logging in using temporary guest accounts or
   existing accounts on arbitrary Matrix homeservers using the Client-Server API.
-}


type alias Authentication =
    { homeserverUrl : String
    , authType : AuthType
    , txnId : Int
    , userId : String
    , accessToken : String
    }


type AuthType
    = Guest
    | User


{-| Decodes "user\_id" and "access\_token" fields into an Authentication record.
Can be used for both /register and /login.
-}
decodeAuthentication : String -> AuthType -> JD.Decoder Authentication
decodeAuthentication homeserverUrl authType =
    JD.map2 (Authentication homeserverUrl authType 0)
        (JD.field "user_id" JD.string)
        (JD.field "access_token" JD.string)


{-| Register a guest account with the homeserver, by sending a HTTP POST to
/register?kind=guest. This presumes that the homeserver has enabled guest registrations.
-}
registerGuest : String -> Task Http.Error Authentication
registerGuest homeserverUrl =
    apiRequest
        { method = "POST"
        , url = clientEndpoint homeserverUrl [ "register" ] [ Url.Builder.string "kind" "guest" ]
        , responseDecoder = decodeAuthentication homeserverUrl Guest
        , accessToken = Nothing
        , body = Http.stringBody "application/json" "{}"
        }


{-| Login by sending a POST request to the /login endpoint
Login type "m.login.password" and identifier type "m.id.user"
-}
login : { homeserverUrl : String, user : String, password : String } -> Task Http.Error Authentication
login { homeserverUrl, user, password } =
    apiRequest
        { method = "POST"
        , url = clientEndpoint homeserverUrl [ "login" ] []
        , responseDecoder = decodeAuthentication homeserverUrl User
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



-- LOGIN FORM


type alias LoginForm =
    { username : String
    , password : String
    , homeserverUrl : String
    }


initLoginForm : LoginForm
initLoginForm =
    { username = ""
    , password = ""
    , homeserverUrl = "https://matrix.org"
    }



-- VIEW


viewLoginButton : msg -> Html msg
viewLoginButton msg =
    button
        [ onClick msg ]
        [ p [] [ text "Log In" ] ]


{-| HTML view for a login form
-}
viewLoginForm : LoginForm -> { submitMsg : LoginForm -> msg, hideMsg : msg } -> Html msg
viewLoginForm loginForm { submitMsg, hideMsg } =
    let
        username =
            labelBefore
                [ class "cactus-login-field" ]
                (p [] [ text "Username:" ])
                (inputText loginForm.username [ placeholder "Username" ])

        password =
            labelBefore
                [ class "cactus-login-field" ]
                (p [] [ text "Password:" ])
                (inputText loginForm.username [ placeholder "Password", type_ "password" ])

        homeserverUrl =
            labelBefore
                [ class "cactus-login-field" ]
                (p [] [ text "Homeserver Url:" ])
                (inputText loginForm.homeserverUrl [ type_ "url" ])

        backButton =
            button
                [ onClick hideMsg ]
                [ p [] [ text "Back" ] ]

        submitButton =
            button
                [ onClick <| submitMsg loginForm ]
                [ p [] [ text "Log in" ] ]

        buttons =
            div
                [ class "cactus-login-buttons" ]
                [ backButton
                , submitButton
                ]
    in
    -- TODO: this might be a good place to put a cactus logo
    div [ class "cactus-login-form" ]
        [ h3 [] [ text "Log in using Matrix" ]
        , username
        , password
        , homeserverUrl
        , buttons
        ]
