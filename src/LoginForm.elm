module LoginForm exposing (FormState(..), LoginForm, initLoginForm, loginWithForm, viewLoginForm)

import Accessibility exposing (Html, a, button, div, h3, h4, inputText, labelBefore, p, text)
import ApiUtils exposing (matrixDotToUrl)
import Html.Attributes exposing (attribute, class, disabled, href, placeholder, required, style, type_)
import Html.Events exposing (onClick, onInput)
import Session exposing (Session, login)
import Svg exposing (path, svg)
import Svg.Attributes as S exposing (d, viewBox)
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
viewLoginForm : LoginForm -> String -> { editMsg : LoginForm -> msg, submitMsg : LoginForm -> msg, hideMsg : msg } -> Html msg
viewLoginForm (LoginForm form) roomAlias { editMsg, submitMsg, hideMsg } =
    let
        closeButton =
            button
                [ class "cactus-login-close"
                , attribute "aria-label" "close"
                , onClick hideMsg
                ]
                [ svg
                    [ viewBox "0 0 20 20"
                    , S.class "cactus-login-close-icon"
                    , style "fill" "currentColor"
                    ]
                    [ path
                        [ style "fill-rule" "evenodd"
                        , d "M4.293 4.293a1 1 0 011.414 0L10 8.586l4.293-4.293a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 01-1.414 1.414L10 11.414l-4.293 4.293a1 1 0 01-1.414-1.414L8.586 10 4.293 5.707a1 1 0 010-1.414z"
                        , style "clip-rule" "evenodd"
                        ]
                        []
                    ]
                ]

        title =
            h3 [ class "cactus-login-title" ]
                [ text "Log in using Matrix" ]

        clientTitle =
            h4 [ class "cactus-login-client-title" ]
                [ text "Use a Matrix client" ]

        clientLink =
            a
                [ class "cactus-button"
                , class "cactus-matrixdotto-button"
                , href <| matrixDotToUrl roomAlias
                ]
                [ text "Log in" ]

        clientForm =
            [ div
                [ class "cactus-login-client" ]
                [ clientTitle
                , clientLink
                ]
            ]

        credentialsTitle =
            h4 [ class "cactus-login-credentials-title" ]
                [ text "Or type in your credentials" ]

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

        submitButton =
            button
                [ class "cactus-button"
                , class "cactus-login-credentials-button"
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

        credentialsForm =
            [ div
                [ class "cactus-login-credentials" ]
                [ credentialsTitle
                , username
                , password
                , homeserverUrl
                , submitButton
                ]
            ]
    in
    div [ class "cactus-login-form-wrapper" ]
        [ div [ class "cactus-login-form" ] <|
            [ closeButton
            , title
            ]
                ++ clientForm
                ++ credentialsForm
        ]
