module LoginForm exposing (LoginForm, Msg, initLoginForm, showLogin, updateLoginForm, viewLoginForm)

import Accessibility exposing (Html, a, button, div, h3, inputText, labelBefore, p, text)
import ApiUtils exposing (UserId, lookupHomeserverUrl, matrixDotToUrl, parseUserId, username)
import Html
import Html.Attributes exposing (class, disabled, href, placeholder, required, type_)
import Html.Events exposing (onClick, onInput)
import Session exposing (Session, login)
import Task exposing (Task)



-- LOGIN FORM


type LoginForm
    = LoginForm
        { userIdField : String
        , userId : Result String UserId
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
        , userId =
            parseUserId "@alice:example.com"
                |> Result.mapError (\_ -> "Something's wrong with the user ID parser")
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
            -- parse userid
            updateState
                { form
                    | userIdField = userIdStr
                    , userId = parseUserId userIdStr
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


{-| A single text field input,
with optional errors and appropriate ARIA tags
-}
textField :
    { name : String
    , value : String
    , placeholder : String
    , msgf : String -> Msg
    , attrs : List (Html.Attribute Msg)
    , error : Maybe String
    }
    -> Html Msg
textField params =
    labelBefore
        [ class "cactus-login-field" ]
        (text params.name)
    <|
        div []
            [ -- the actual text field
              inputText params.value <|
                (params.attrs
                    ++ [ placeholder params.placeholder
                       , onInput params.msgf
                       , required True
                       ]
                 -- TODO: aria fields
                )
            , -- optional error
              params.error
                -- TODO: style this
                |> Maybe.map (text >> List.singleton >> p [])
                |> Maybe.withDefault (text "")
            ]


{-| HTML view for a login form.
-}
viewLoginForm : LoginForm -> String -> Html Msg
viewLoginForm (LoginForm form) roomAlias =
    let
        username =
            textField
                { name = "User ID"
                , placeholder = "@alice:example.com"
                , value = form.userIdField
                , msgf = EditUserId
                , attrs = []
                , error =
                    case form.userId of
                        Err e ->
                            Just e

                        _ ->
                            Nothing
                }

        password =
            textField
                { name = "Password"
                , placeholder = "••••••••••••"
                , value = form.passwordField
                , msgf = EditPassword
                , attrs = [ type_ "password" ]
                , error = Nothing
                }

        homeserverUrl =
            textField
                { name = "Homeserver Url"
                , placeholder = "Homeserver Url"
                , value = form.homeserverUrlField |> Maybe.withDefault ""
                , msgf = EditHomeserverUrl
                , attrs = []
                , error = Nothing
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
                    ++ (form.userId
                            |> Result.map (Login >> onClick >> List.singleton)
                            |> Result.withDefault []
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
