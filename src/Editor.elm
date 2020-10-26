module Editor exposing (Editor, joinPutLeave, viewEditor)

import ApiUtils exposing (apiRequest, clientEndpoint, matrixDotToUrl)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as JD
import Json.Encode as JE
import Task exposing (Task)



-- EDITOR


type alias Editor =
    { homeserverUrl : String
    , accessToken : String
    , content : String
    , txnId : Int
    }


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


viewEditor : { editMsg : String -> msg, sendMsg : Editor -> msg, roomAlias : String, editor : Editor } -> Html msg
viewEditor { editMsg, sendMsg, roomAlias, editor } =
    div
        [ class "cactus-editor" ]
        [ a
            [ href <| matrixDotToUrl roomAlias ]
            [ text "Join via another client" ]
        , textarea
            [ onInput editMsg
            , value editor.content
            ]
            []
        , button
            [ onClick <| sendMsg editor ]
            [ text "Send" ]
        ]
