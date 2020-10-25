module Main exposing (main)

import ApiUtils exposing (apiRequest, clientEndpoint, matrixDotToUrl)
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
import Register exposing (registerGuest)
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
    , roomState : Maybe { room : Room, editor : Editor.Editor }
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
      , roomState = Nothing
      , error = Nothing
      }
    , Task.attempt GotRoom <| getInitialRoom config
    )


type Msg
    = GotRoom (Result Http.Error Room)
    | ViewMoreClicked
    | GotMessages (Result Http.Error GetMessagesResponse)
      -- EDITOR
    | EditComment String
    | SendComment String Editor
    | SentComment (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.roomState ) of
        ( GotRoom (Ok room), _ ) ->
            -- got initial room, when loading page first ime
            ( { model
                | roomState =
                    Just
                        -- init both room and editor. this enables most of the ui
                        { room = room
                        , editor =
                            { homeserverUrl = model.config.defaultHomeserverUrl
                            , accessToken = room.accessToken
                            , content = ""
                            , txnId = 0
                            , joined = False
                            }
                        }
              }
            , Cmd.none
            )

        ( GotRoom (Err err), _ ) ->
            -- error while setting up initial room
            ( { model | error = Just <| Debug.toString err }
            , Cmd.none
            )

        ( ViewMoreClicked, Just roomState ) ->
            -- "view more" button hit - issue request to fetch more messages
            ( model
            , Task.attempt GotMessages <|
                getMessages
                    { homeserverUrl = model.config.defaultHomeserverUrl
                    , accessToken = roomState.room.accessToken
                    , roomId = roomState.room.roomId
                    , from = roomState.room.end
                    }
            )

        ( ViewMoreClicked, _ ) ->
            -- impossible state:
            -- "view more" clicked - but the room hasn't finished loading yet.
            ( { model | error = Just "Can't fetch messages: no connection to homeserver" }
            , Cmd.none
            )

        ( GotMessages (Ok newMsgs), Just roomState ) ->
            -- got more messages. result of the "ViewMore"-button request
            let
                newRoom =
                    mergeNewMessages roomState.room newMsgs

                newRoomState =
                    { roomState | room = newRoom }
            in
            ( { model | roomState = Just newRoomState }
            , Cmd.none
            )

        ( GotMessages (Err httpErr), Just room ) ->
            -- http error while getting more comments
            ( { model | error = Just <| Debug.toString httpErr }
            , Cmd.none
            )

        ( GotMessages newMsgs, Nothing ) ->
            -- impossible state: get response with new messages,
            -- without having an initialized room
            ( { model | error = Just "Unexpected state: got message response without a room" }
            , Cmd.none
            )

        ( EditComment str, Just roomState ) ->
            -- user changes text in comment box
            let
                editor =
                    roomState.editor

                newEditor =
                    { editor | content = str }

                newRoomState =
                    { roomState | editor = newEditor }
            in
            ( { model | roomState = Just newRoomState }
            , Cmd.none
            )

        ( EditComment _, Nothing ) ->
            ( { model | error = Just "Impossible state: can't edit a text field that doesn't exist" }, Cmd.none )

        ( SendComment roomId editor, _ ) ->
            -- user hit send button
            let
                newEditor =
                    { editor
                        | txnId = editor.txnId + 1
                        , joined = True
                        , content = ""
                    }

                newRoomState =
                    Maybe.map (\rs -> { rs | editor = newEditor }) model.roomState
            in
            ( { model | roomState = newRoomState }
            , Task.attempt SentComment <|
                joinPutLeave
                    { homeserverUrl = editor.homeserverUrl
                    , accessToken = editor.accessToken
                    , roomId = roomId
                    , txnId = editor.txnId
                    , body = editor.content
                    }
            )

        ( SentComment (Ok ()), _ ) ->
            ( model
            , Cmd.none
            )

        ( SentComment (Err httpErr), _ ) ->
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
                        , sendMsg = SendComment roomState.room.roomId
                        , roomAlias = roomState.room.roomAlias
                        , editor = roomState.editor
                        }
                    , viewRoomEvents
                        model.config.defaultHomeserverUrl
                        roomState.room.time
                        roomState.room.members
                        roomState.room.events
                    , viewMoreButton
                    ]
        ]


viewRoomEvents : String -> Time.Posix -> Dict String Member -> List RoomEvent -> Html Msg
viewRoomEvents defaultHomeserverUrl time members roomEvents =
    div [] <|
        List.map
            (viewMessageEvent defaultHomeserverUrl time members)
            (onlyMessageEvents roomEvents)


viewMoreButton : Html Msg
viewMoreButton =
    button
        [ onClick ViewMoreClicked ]
        [ text "View more" ]
