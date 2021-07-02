module LoginForm exposing (FormState(..), LoginForm, Msg, initLoginForm, showLogin, updateLoginForm, viewLoginForm)

import Accessibility exposing (Html, a, button, div, h3, inputText, labelBefore, p, text)
import ApiUtils exposing (UserId, lookupHomeserverUrl, matrixDotToUrl, parseUserId, username)
import Html.Attributes exposing (class, disabled, href, placeholder, required, type_)
import Html.Events exposing (onClick, onInput)
import Session exposing (Session, login)
import Task exposing (Task)



-- LOGIN FORM


type LoginForm
    = LoginForm
        { userIdField : String
        , userIdError : Maybe String
        , passwordField : String
        , homeserverUrlField : Maybe String
        , loginError : Maybe LoginError
        , state : FormState
        }


type FormState
    = Ready
    | LoggingIn
    | Hidden


initLoginForm : LoginForm
initLoginForm =
    LoginForm
        { userIdField = ""
        , userIdError = Nothing
        , passwordField = ""
        , homeserverUrlField = Nothing
        , loginError = Nothing
        , state = Hidden
        }


type Msg
    = EditUserId String
    | EditPassword String
    | EditHomeserverUrl String
    | HideLogin
    | Login UserId
    | LoggedIn (Result LoginError Session)


updateLoginForm : LoginForm -> Msg -> ( LoginForm, Cmd Msg, Maybe Session )
updateLoginForm (LoginForm form) msg =
    let
        updateState f =
            ( LoginForm f, Cmd.none, Nothing )
    in
    case msg of
        EditPassword password ->
            updateState { form | passwordField = password }

        EditHomeserverUrl homeserverUrl ->
            updateState { form | homeserverUrlField = Just homeserverUrl }

        EditUserId userIdStr ->
            -- parse userid and show error if relevant
            let
                userId : Maybe UserId
                userId =
                    parseUserId userIdStr
            in
            updateState
                { form
                    | userIdField = userIdStr
                    , userIdError =
                        Maybe.map
                            (\_ -> "User ID should be in the format: @user:server.com")
                            userId
                }

        HideLogin ->
            updateState { form | state = Hidden }

        Login userId ->
            ( LoginForm { form | state = LoggingIn }
            , Task.attempt LoggedIn <| loginWithForm (LoginForm form) userId
            , Nothing
            )

        LoggedIn (Err err) ->
            -- login failed. show error
            updateState { form | loginError = Just err }

        LoggedIn (Ok sess) ->
            -- Success! Reset login form and pass session up to caller
            ( initLoginForm
            , Cmd.none
            , Just sess
            )


type LoginError
    = HomeserverLookupFailed String
    | LoginFailed Session.Error


{-| Login with the data from a loginform
First look up the homeserver using .well-known
then log in using that homeserver url.
-}
loginWithForm : LoginForm -> UserId -> Task LoginError Session
loginWithForm (LoginForm form) userId =
    lookupHomeserverUrl userId
        |> Task.mapError HomeserverLookupFailed
        |> Task.andThen
            (\homeserverUrl ->
                login
                    { homeserverUrl = homeserverUrl
                    , user = username userId
                    , password = form.passwordField
                    }
                    |> Task.mapError LoginFailed
            )


showLogin : LoginForm -> LoginForm
showLogin (LoginForm form) =
    if form.state == Hidden then
        LoginForm { form | state = Ready }

    else
        LoginForm form



-- VIEW


{-| HTML view for a login form.
-}
viewLoginForm : LoginForm -> String -> Html Msg
viewLoginForm (LoginForm form) roomAlias =
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
                { name = "User ID"
                , placeholder = "@alice:example.com"
                , value = form.userIdField
                , msgf = EditUserId
                , attrs = []
                }

        password =
            textField
                { name = "Password"
                , placeholder = "password"
                , value = form.passwordField
                , msgf = EditPassword
                , attrs = [ type_ "password" ]
                }

        homeserverUrl =
            textField
                { name = "Homeserver Url"
                , placeholder = "Homeserver Url"
                , value = form.homeserverUrlField |> Maybe.withDefault ""
                , msgf = EditHomeserverUrl
                , attrs = []
                }

        backButton =
            button
                [ class "cactus-button"
                , onClick HideLogin
                ]
                [ text "Back" ]

        submitButton =
            button
                ([ class "cactus-button"
                 , disabled <| not (form.state == Ready)
                 ]
                    ++ (case parseUserId form.userIdField of
                            Just uid ->
                                [ onClick <| Login uid ]

                            _ ->
                                []
                       )
                )
                [ text <|
                    case form.state of
                        Ready ->
                            "Log in"

                        LoggingIn ->
                            "Logging in..."

                        Hidden ->
                            ""
                ]

        anotherClientLink =
            a
                [ class "cactus-button"
                , class "cactus-matrixdotto-button"
                , href <| matrixDotToUrl roomAlias
                ]
                [ text "Use a Matrix client" ]

        buttons =
            [ div
                [ class "cactus-login-buttons" ]
                [ backButton
                , submitButton
                ]
            , anotherClientLink
            ]
    in
    case form.state of
        Hidden ->
            text ""

        _ ->
            div [ class "cactus-login-form" ] <|
                [ h3 [] [ text "Log in using Matrix" ]
                , username
                , password
                ]
                    ++ buttons
