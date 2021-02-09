module Editor exposing (viewEditor)

import Accessibility exposing (Html, a, button, div, labelHidden, p, text, textarea)
import ApiUtils exposing (matrixDotToUrl)
import Html.Attributes exposing (class, disabled, href, value)
import Html.Events exposing (onClick, onInput)
import Session exposing (Kind(..), Session, isUser, sessionStatusString)



{- EDITOR
   Handles UI for the comment editor and buttons.
-}


viewEditor :
    { showLoginMsg : msg
    , logoutMsg : msg
    , editMsg : String -> msg
    , sendMsg : Maybe msg
    , session : Maybe Session
    , roomAlias : String
    , editorContent : String
    }
    -> Html msg
viewEditor { session, showLoginMsg, logoutMsg, editMsg, sendMsg, roomAlias, editorContent } =
    let
        anotherClientLink =
            a
                [ href <| matrixDotToUrl roomAlias ]
                [ text "Use a Matrix client" ]

        commentEditor =
            labelHidden
                "Comment Editor"
                []
                (text "Comment Editor")
                (textarea
                    [ class "cactus-editor-textarea"
                    , value editorContent
                    , onInput editMsg
                    ]
                    []
                )

        sendButton =
            viewSendButton sendMsg session editorContent

        authStatusStr =
            session
                |> Maybe.map sessionStatusString
                |> Maybe.withDefault "Connecting to Matrix server..."

        signedInText =
            p [] [ text authStatusStr ]
    in
    div
        [ class "cactus-editor" ]
        [ div [ class "cactus-editor-above" ] [ signedInText ]
        , commentEditor
        , div [ class "cactus-editor-below" ]
            [ anotherClientLink
            , div []
                [ loginOrLogoutButton
                    { loginMsg = showLoginMsg
                    , logoutMsg = logoutMsg
                    , session = session
                    }
                , sendButton
                ]
            ]
        ]


{-| If logged in as a non-guest user, show logout button, else show login
button.
-}
loginOrLogoutButton : { loginMsg : msg, logoutMsg : msg, session : Maybe Session } -> Html msg
loginOrLogoutButton { loginMsg, logoutMsg, session } =
    let
        loginButton =
            button
                [ class "cactus-button"
                , class "cactus-login-button"
                , onClick loginMsg
                ]
                [ text "Log in" ]

        logoutButton =
            button
                [ class "cactus-button"
                , class "cactus-logout-button"
                , onClick logoutMsg
                ]
                [ text "Log out" ]
    in
    case session of
        Just sess ->
            if isUser sess then
                logoutButton

            else
                loginButton

        Nothing ->
            loginButton


viewSendButton : Maybe msg -> Maybe Session -> String -> Html msg
viewSendButton msg auth editorContent =
    let
        -- button is disabled if there is no session
        -- or if editor is empty
        isDisabled : Bool
        isDisabled =
            (auth == Nothing) || (String.length editorContent == 0)

        attrs =
            [ class "cactus-button"
            , class "cactus-send-button"
            , disabled isDisabled
            ]
                -- append onClick message if we can
                ++ (msg
                        |> Maybe.map (\m -> [ onClick m ])
                        |> Maybe.withDefault []
                   )
    in
    button attrs [ text "Post" ]
