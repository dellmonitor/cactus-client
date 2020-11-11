module Editor exposing (Editor, joinPut, joinPutLeave, viewEditor)

import Accessibility exposing (Html, a, button, div, labelHidden, p, text, textarea)
import ApiUtils exposing (matrixDotToUrl)
import Html.Attributes exposing (class, disabled, href, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as JD
import Json.Encode as JE
import Session exposing (Kind(..), Session, authenticatedRequest, isUser, sessionStatusString, transactionId)
import Task exposing (Task)



{- EDITOR
   This module handles the comment editing and posting UI and API interaction.
-}


type alias Editor =
    { content : String }


{-| Chain of three requests:

1.  join room
2.  HTTP PUT a comment into the room
3.  leave the room

-}
joinPutLeave : Session -> String -> String -> Task Session.Error ()
joinPutLeave session roomId comment =
    joinPut session roomId comment
        |> Task.andThen (\_ -> leaveRoom session roomId)


{-| Chain of two requests:

1.  join room
2.  HTTP PUT a comment into the room

-}
joinPut : Session -> String -> String -> Task Session.Error ()
joinPut session roomId comment =
    joinRoom session roomId
        |> Task.andThen (\_ -> putMessage session roomId comment)


joinRoom : Session -> String -> Task Session.Error ()
joinRoom session roomId =
    authenticatedRequest
        session
        { method = "POST"
        , path = [ "rooms", roomId, "join" ]
        , params = []
        , responseDecoder = JD.succeed ()
        , body = Http.stringBody "application/json" "{}"
        }


leaveRoom : Session -> String -> Task Session.Error ()
leaveRoom session roomId =
    authenticatedRequest
        session
        { method = "POST"
        , path = [ "rooms", roomId, "leave" ]
        , params = []
        , responseDecoder = JD.succeed ()
        , body = Http.stringBody "application/json" "{}"
        }


putMessage : Session -> String -> String -> Task Session.Error ()
putMessage session roomId comment =
    -- post a message
    let
        eventType =
            "m.room.message"

        msgtype =
            "m.text"

        txnId =
            transactionId session
    in
    authenticatedRequest
        session
        { method = "PUT"
        , path = [ "rooms", roomId, "send", eventType, String.fromInt txnId ]
        , params = []
        , responseDecoder = JD.succeed ()
        , body =
            Http.jsonBody <|
                JE.object
                    [ ( "msgtype", JE.string msgtype )
                    , ( "body", JE.string comment )
                    ]
        }


viewEditor :
    { showLoginMsg : msg
    , logoutMsg : msg
    , editMsg : String -> msg
    , sendMsg : Maybe msg
    , session : Maybe Session
    , roomAlias : String
    , editor : Editor
    }
    -> Html msg
viewEditor { session, showLoginMsg, logoutMsg, editMsg, sendMsg, roomAlias, editor } =
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
                    , value editor.content
                    , onInput editMsg
                    ]
                    []
                )

        sendButton =
            viewSendButton sendMsg session editor

        authStatusStr =
            session
                |> Maybe.map sessionStatusString
                |> Maybe.withDefault "Connecting to Matrix server..."

        signedInText =
            p [] [ text authStatusStr ]
    in
    div
        [ class "cactus-editor" ]
        [ div [ class "cactus-editor-above" ]
            [ loginOrLogoutButton
                { loginMsg = showLoginMsg
                , logoutMsg = logoutMsg
                , session = session
                }
            , signedInText
            ]
        , commentEditor
        , div [ class "cactus-editor-below" ]
            [ anotherClientLink
            , sendButton
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
                , onClick loginMsg
                ]
                [ text "Log in" ]

        logoutButton =
            button
                [ class "cactus-button"
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


viewSendButton : Maybe msg -> Maybe Session -> Editor -> Html msg
viewSendButton msg auth editor =
    let
        -- button is disabled if there is no session
        -- or if editor is empty
        isDisabled : Bool
        isDisabled =
            (auth == Nothing) || (String.length editor.content == 0)

        attrs =
            [ class "cactus-button"
            , disabled isDisabled
            ]
                -- append onClick message if we can
                ++ (msg
                        |> Maybe.map (\m -> [ onClick m ])
                        |> Maybe.withDefault []
                   )
    in
    button attrs [ text "Post" ]
