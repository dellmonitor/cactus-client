module Main exposing (main)

import ApiUtils exposing (apiRequest, clientEndpoint, matrixDotToUrl)
import Authentication exposing (Authentication)
import Browser
import Dict exposing (Dict)
import Editor exposing (Editor, joinPutLeave, viewEditor)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as JD
import Json.Encode as JE
import Member exposing (Member)
import Message exposing (Event, GetMessagesResponse, Message(..), RoomEvent, getMessages, onlyMessageEvents, viewMessageEvent)
import Room exposing (Room, getInitialRoom, mergeNewMessages)
import Task exposing (Task)
import Time


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \model -> Sub.none
        }


type alias Model =
    { config : StaticConfig
    , editor : Editor
    , roomState : Maybe { room : Room, auth : Authentication }
    , error : Maybe String
    }


type alias StaticConfig =
    { defaultHomeserverUrl : String
    , siteName : String
    , uniqueId : String
    }


init : StaticConfig -> ( Model, Cmd Msg )
init config =
    ( { config = config
      , editor = { content = "" }
      , roomState = Nothing
      , error = Nothing
      }
    , Task.attempt GotRoom <| getInitialRoom config
    )


type Msg
    = GotRoom (Result Http.Error ( Authentication, Room ))
    | ViewMoreClicked Authentication Room
    | GotMessages Room (Result Http.Error GetMessagesResponse)
      -- EDITOR
    | EditComment String
    | SendComment Authentication Room
    | SentComment (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRoom (Ok ( auth, room )) ->
            -- got initial room, when loading page first ime
            ( { model
                | roomState =
                    Just
                        -- init both room and editor. this enables most of the ui
                        { room = room
                        , auth = auth
                        }
              }
            , Cmd.none
            )

        GotRoom (Err err) ->
            -- error while setting up initial room
            ( { model | error = Just <| Debug.toString err }
            , Cmd.none
            )

        ViewMoreClicked auth room ->
            -- "view more" button hit - issue request to fetch more messages
            ( model
            , Task.attempt (GotMessages room) <|
                getMessages
                    { homeserverUrl = auth.homeserverUrl
                    , accessToken = auth.accessToken
                    , roomId = room.roomId
                    , from = room.end
                    }
            )

        GotMessages room (Ok newMsgs) ->
            -- got more messages. result of the "ViewMore"-button request
            let
                newRoom =
                    mergeNewMessages room newMsgs

                newRoomState =
                    Maybe.map
                        (\rs -> { rs | room = newRoom })
                        model.roomState
            in
            ( { model | roomState = newRoomState }
            , Cmd.none
            )

        GotMessages _ (Err httpErr) ->
            -- http error while getting more comments
            ( { model | error = Just <| Debug.toString httpErr }
            , Cmd.none
            )

        EditComment str ->
            -- user changes text in comment box
            ( { model | editor = { content = str } }
            , Cmd.none
            )

        SendComment auth room ->
            -- user hit send button
            let
                -- increment transaction Id (idempotency measure)
                newAuth =
                    { auth | txnId = auth.txnId + 1 }
            in
            ( { model
                | editor = { content = "" }
                , roomState = Just { auth = newAuth, room = room }
              }
            , Task.attempt SentComment <|
                joinPutLeave
                    { homeserverUrl = auth.homeserverUrl
                    , accessToken = auth.accessToken
                    , txnId = auth.txnId
                    , roomId = room.roomId
                    , body = model.editor.content
                    }
            )

        SentComment (Ok _) ->
            ( model
              -- TODO: fetch messages again
            , Cmd.none
            )

        SentComment (Err httpErr) ->
            ( { model | error = Just <| Debug.toString httpErr }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "cactus-container" ] <|
        [ -- view errors
          h5 [] <|
            case model.error of
                Nothing ->
                    []

                Just errmsg ->
                    [ text <| "ERROR: " ++ errmsg ]
        , -- editor and comments section
          case model.roomState of
            Nothing ->
                p [] [ text "Getting comments..." ]

            Just roomState ->
                div []
                    [ viewEditor
                        { editMsg = EditComment
                        , sendMsg = SendComment roomState.auth roomState.room
                        , roomAlias = roomState.room.roomAlias
                        , editor = model.editor
                        }
                    , viewRoomEvents
                        model.config.defaultHomeserverUrl
                        roomState.room.time
                        roomState.room.members
                        roomState.room.events
                    , viewMoreButton roomState.auth roomState.room
                    ]
        ]


viewRoomEvents : String -> Time.Posix -> Dict String Member -> List RoomEvent -> Html Msg
viewRoomEvents defaultHomeserverUrl time members roomEvents =
    div [] <|
        List.map
            (viewMessageEvent defaultHomeserverUrl time members)
            (onlyMessageEvents roomEvents)


viewMoreButton : Authentication -> Room -> Html Msg
viewMoreButton auth room =
    button
        [ onClick (ViewMoreClicked auth room) ]
        [ text "View more" ]
