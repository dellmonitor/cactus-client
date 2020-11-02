module Editor exposing (Editor, joinPutLeave, viewEditor)

import Accessibility exposing (Html, a, button, div, labelHidden, p, text, textarea)
import ApiUtils exposing (apiRequest, clientEndpoint, matrixDotToUrl)
import Authentication exposing (AuthType(..), Authentication, authStatusString)
import Html.Attributes exposing (class, disabled, href, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as JD
import Json.Encode as JE
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
joinPutLeave : { homeserverUrl : String, accessToken : String, roomId : String, txnId : Int, body : String } -> Task Http.Error ()
joinPutLeave config =
    joinRoom config
        |> Task.andThen (\_ -> putMessage config)
        |> Task.andThen (\_ -> leaveRoom config)


joinRoom : { a | homeserverUrl : String, accessToken : String, roomId : String } -> Task Http.Error ()
joinRoom { homeserverUrl, accessToken, roomId } =
    apiRequest
        { method = "POST"
        , url = clientEndpoint homeserverUrl [ "rooms", roomId, "join" ] []
        , accessToken = Just accessToken
        , responseDecoder = JD.succeed ()
        , body = Http.stringBody "application/json" "{}"
        }


leaveRoom : { a | homeserverUrl : String, accessToken : String, roomId : String } -> Task Http.Error ()
leaveRoom { homeserverUrl, accessToken, roomId } =
    apiRequest
        { method = "POST"
        , url = clientEndpoint homeserverUrl [ "rooms", roomId, "leave" ] []
        , accessToken = Just accessToken
        , responseDecoder = JD.succeed ()
        , body = Http.stringBody "application/json" "{}"
        }


putMessage : { a | homeserverUrl : String, accessToken : String, roomId : String, txnId : Int, body : String } -> Task Http.Error ()
putMessage { homeserverUrl, accessToken, roomId, txnId, body } =
    -- post a message
    let
        eventType =
            "m.room.message"

        msgtype =
            "m.text"
    in
    apiRequest
        { method = "PUT"
        , url =
            clientEndpoint homeserverUrl
                [ "rooms", roomId, "send", eventType, String.fromInt txnId ]
                []
        , accessToken = Just accessToken
        , responseDecoder = JD.succeed ()
        , body =
            Http.jsonBody <|
                JE.object
                    [ ( "msgtype", JE.string msgtype )
                    , ( "body", JE.string body )
                    ]
        }


viewEditor :
    { showLoginMsg : msg
    , editMsg : String -> msg
    , sendMsg : Maybe msg
    , auth : Maybe Authentication
    , roomAlias : String
    , editor : Editor
    }
    -> Html msg
viewEditor { auth, showLoginMsg, editMsg, sendMsg, roomAlias, editor } =
    let
        loginButton =
            button
                [ class "cactus-button"
                , onClick showLoginMsg
                ]
                [ text "Log in using Matrix" ]

        anotherClientLink =
            button
                [ class "cactus-button"
                , href <| matrixDotToUrl roomAlias
                ]
                [ text "Join using a Matrix client" ]

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
            viewSendButton sendMsg auth editor

        authStatusStr =
            auth
                |> Maybe.map authStatusString
                |> Maybe.withDefault "Trying to connect to Matrix homeserver..."

        signedInText =
            p [] [ text authStatusStr ]
    in
    div
        [ class "cactus-editor" ]
        [ div [ class "cactus-editor-above" ]
            [ anotherClientLink
            , loginButton
            ]
        , commentEditor
        , div [ class "cactus-editor-below" ]
            -- TODO
            [ signedInText
            , sendButton
            ]
        ]


viewSendButton : Maybe msg -> Maybe Authentication -> Editor -> Html msg
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
    button attrs [ text "Post comment" ]
