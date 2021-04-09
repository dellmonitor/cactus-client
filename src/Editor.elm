module Editor exposing (viewEditor)

import Accessibility exposing (Html, a, button, div, labelHidden, text, textarea)
import ApiUtils exposing (matrixDotToUrl)
import Html.Attributes exposing (class, disabled, href, placeholder, value)
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
    , loginEnabled : Bool
    , guestPostingEnabled : Bool
    }
    -> Html msg
viewEditor { session, showLoginMsg, logoutMsg, editMsg, sendMsg, roomAlias, editorContent, loginEnabled, guestPostingEnabled } =
    let
        commentEditor enabled =
            labelHidden
                "Comment Editor"
                []
                (text "Comment Editor")
                (textarea
                    [ class "cactus-editor-textarea"
                    , value editorContent
                    , onInput editMsg
                    , placeholder "Add a comment"
                    , disabled <| not enabled
                    ]
                    []
                )

        sendButton =
            viewSendButton sendMsg session editorContent

        loginButton =
            loginOrLogoutButton
                { loginMsg = showLoginMsg
                , logoutMsg = logoutMsg
                , session = session
                }
    in
    div [ class "cactus-editor" ] <|
        case ( loginEnabled, guestPostingEnabled ) of
            ( True, True ) ->
                -- fully featured
                [ commentEditor True
                , div
                    [ class "cactus-editor-below" ]
                    [ loginButton, sendButton ]
                ]

            ( True, False ) ->
                -- disable guest posting
                [ commentEditor <| (Maybe.map isUser session |> Maybe.withDefault False)
                , div
                    [ class "cactus-editor-below" ]
                    [ loginButton, sendButton ]
                ]

            ( False, True ) ->
                -- replace login button with matrix.to button
                [ commentEditor True
                , div
                    [ class "cactus-editor-below" ]
                    [ a
                        [ href <| matrixDotToUrl roomAlias ]
                        [ button
                            [ class "cactus-button" ]
                            [ text "Log in" ]
                        ]
                    , sendButton
                    ]
                ]

            ( False, False ) ->
                -- only show matrix.to button
                [ a
                    [ href <| matrixDotToUrl roomAlias ]
                    [ button
                        [ class "cactus-button"
                        , class "cactus-matrixdotto-only"
                        ]
                        [ text "Comment using a Matrix client" ]
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
