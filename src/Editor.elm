module Editor exposing (viewEditor)

import Accessibility exposing (Html, button, div, labelHidden, text, textarea)
import Html.Attributes exposing (class, disabled, value)
import Html.Events exposing (onClick, onInput)
import Session exposing (Kind(..), Session, getUserId, isUser)



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
viewEditor { session, showLoginMsg, logoutMsg, editMsg, sendMsg, editorContent } =
    let
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
    in
    div
        [ class "cactus-editor" ]
        [ commentEditor
        , div [ class "cactus-editor-below" ]
            [ div []
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

        postButtonString =
            case auth of
                Nothing ->
                    -- greyed out, since it is disabled
                    "Post"

                Just session ->
                    "Post as " ++ getUserId session
    in
    button attrs [ text postButtonString ]
