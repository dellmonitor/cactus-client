module Editor exposing (Editor, setContent, setName, viewEditor)

import Accessibility exposing (Html, a, button, div, inputText, labelHidden, text, textarea)
import ApiUtils exposing (matrixDotToUrl)
import Html.Attributes exposing (class, disabled, href, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Session exposing (Kind(..), Session, getUserId, isUser)



{- EDITOR
   Handles UI for the comment editor and buttons.
-}


type alias Editor =
    { comment : String
    , name : String
    }


setContent : Editor -> String -> Editor
setContent editor content =
    { editor | comment = content }


setName : Editor -> String -> Editor
setName editor name =
    { editor | name = name }


viewEditor :
    { session : Maybe Session
    , editor : Editor
    , showLoginMsg : msg
    , logoutMsg : msg
    , editMsg : String -> msg
    , sendMsg : Maybe msg
    , nameMsg : String -> msg
    , roomAlias : String
    , loginEnabled : Bool
    , guestPostingEnabled : Bool
    }
    -> Html msg
viewEditor { session, editor, showLoginMsg, logoutMsg, editMsg, sendMsg, nameMsg, roomAlias, loginEnabled, guestPostingEnabled } =
    let
        commentEditor enabled =
            labelHidden
                "Comment Editor"
                []
                (text "Comment Editor")
                (textarea
                    [ class "cactus-editor-textarea"
                    , value editor.comment
                    , onInput editMsg
                    , placeholder "Add a comment"
                    , disabled <| not enabled
                    ]
                    []
                )

        sendButton =
            viewSendButton sendMsg session editor.comment

        loginButton =
            if loginEnabled then
                loginOrLogoutButton
                    { loginMsg = showLoginMsg
                    , logoutMsg = logoutMsg
                    , session = session
                    }

            else
                a
                    [ href <| matrixDotToUrl roomAlias ]
                    [ button [ class "cactus-button" ] [ text "Log in" ] ]

        buttonDiv =
            div [ class "cactus-editor-buttons" ] [ loginButton, sendButton ]

        nameInput =
            if Maybe.map (isUser >> not) session |> Maybe.withDefault True then
                div [ class "cactus-editor-name" ] <|
                    [ labelHidden
                        "Name"
                        []
                        (text "Name")
                        (inputText editor.name
                            [ placeholder "Name"
                            , onInput nameMsg
                            ]
                        )
                    ]

            else
                text ""
    in
    div [ class "cactus-editor" ] <|
        case ( loginEnabled, guestPostingEnabled ) of
            ( True, True ) ->
                -- fully featured
                [ commentEditor True
                , div
                    [ class "cactus-editor-below" ]
                    [ nameInput, buttonDiv ]
                ]

            ( True, False ) ->
                -- disable guest posting, also disables username field
                [ commentEditor <| (Maybe.map isUser session |> Maybe.withDefault False)
                , div
                    [ class "cactus-editor-below" ]
                    [ buttonDiv ]
                ]

            ( False, True ) ->
                -- disable login (replace login button with matrix.to button)
                [ commentEditor True
                , div
                    [ class "cactus-editor-below" ]
                    [ nameInput, buttonDiv ]
                ]

            ( False, False ) ->
                -- no posting. only show matrix.to button
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
viewSendButton msg session editorContent =
    let
        -- button is disabled if there is no session
        -- or if editor is empty
        isDisabled : Bool
        isDisabled =
            (session == Nothing) || (String.length editorContent == 0)

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
            case ( Maybe.map isUser session, Maybe.map getUserId session ) of
                ( Just True, Just userid ) ->
                    -- when signed in: show matrix user id on button
                    "Post as " ++ userid

                _ ->
                    -- when unauthenticated or guest
                    "Post"
    in
    button attrs [ text postButtonString ]
