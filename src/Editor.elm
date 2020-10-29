module Editor exposing (Editor, joinPutLeave, viewEditor)

import Accessibility exposing (Html, a, button, div, labelHidden, text, textarea)
import ApiUtils exposing (apiRequest, clientEndpoint, matrixDotToUrl)
import Html.Attributes exposing (class, href, type_, value)
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


viewEditor : { editMsg : String -> msg, sendMsg : msg, roomAlias : String, editor : Editor } -> Html msg
viewEditor { editMsg, sendMsg, roomAlias, editor } =
    let
        commentEditor =
            labelHidden
                "cactus-comment-editor"
                []
                (text "Comment Editor")
                (textarea
                    [ value editor.content
                    , onInput editMsg
                    ]
                    []
                )
    in
    div
        [ class "cactus-editor" ]
        [ a
            [ href <| matrixDotToUrl roomAlias ]
            [ text "Join via another client" ]
        , commentEditor
        , button
            [ onClick sendMsg ]
            [ text "Send" ]
        ]
