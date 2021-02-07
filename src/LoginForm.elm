module LoginForm exposing (FormState(..), LoginForm, initLoginForm, loginWithForm, viewLoginForm)

import Accessibility exposing (Html, button, div, h3, inputText, labelBefore, p, text)
import Html.Attributes exposing (class, disabled, placeholder, required, type_)
import Html.Events exposing (onClick, onInput)
import Session exposing (Session, login)
import Task exposing (Task)



-- LOGIN FORM


type LoginForm
    = LoginForm
        { username : String
        , password : String
        , homeserverUrl : String
        , state : FormState
        }


type FormState
    = Ready
    | LoggingIn


initLoginForm : LoginForm
initLoginForm =
    LoginForm
        { username = ""
        , password = ""
        , homeserverUrl = "https://matrix.org"
        , state = Ready
        }


isValid : { a | username : String, password : String, homeserverUrl : String } -> Bool
isValid { username, password } =
    (username /= "") && (password /= "")


loginWithForm : LoginForm -> ( LoginForm, Task Session.Error Session )
loginWithForm (LoginForm form) =
    ( LoginForm { form | state = LoggingIn }
    , login
        { homeserverUrl = form.homeserverUrl
        , user = form.username
        , password = form.password
        }
    )



-- VIEW


{-| HTML view for a login form.
-}
viewLoginForm : LoginForm -> { editMsg : LoginForm -> msg, submitMsg : LoginForm -> msg, hideMsg : msg } -> Html msg
viewLoginForm (LoginForm form) { editMsg, submitMsg, hideMsg } =
    let
        textField { name, value, msgf, attrs } =
            labelBefore
                [ class "cactus-login-field" ]
                (p [] [ text name ])
                (inputText value <|
                    [ placeholder name
                    , onInput msgf
                    , required True
                    ]
                        ++ attrs
                )

        username =
            textField
                { name = "Username"
                , value = form.username
                , msgf = \str -> editMsg (LoginForm { form | username = str })
                , attrs = []
                }

        password =
            textField
                { name = "Password"
                , value = form.password
                , msgf = \str -> editMsg (LoginForm { form | password = str })
                , attrs = [ type_ "password" ]
                }

        homeserverUrl =
            textField
                { name = "Homeserver Url"
                , value = form.homeserverUrl
                , msgf = \str -> editMsg (LoginForm { form | homeserverUrl = str })
                , attrs = []
                }

        backButton =
            button
                [ class "cactus-button"
                , onClick hideMsg
                ]
                [ text "Back" ]

        submitButton =
            button
                [ class "cactus-button"
                , onClick <| submitMsg (LoginForm form)
                , disabled <| not (isValid form && form.state == Ready)
                ]
                [ text <|
                    case form.state of
                        Ready ->
                            "Log in"

                        LoggingIn ->
                            "Logging in..."
                ]

        buttons =
            div
                [ class "cactus-login-buttons" ]
                [ backButton
                , submitButton
                ]
    in
    div [ class "cactus-login-form" ]
        [ h3 [] [ text "Log in using Matrix" ]
        , username
        , password
        , homeserverUrl
        , buttons
        ]
