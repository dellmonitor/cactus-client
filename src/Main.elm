module Main exposing (main)

import ApiUtils exposing (mxcToHttp)
import Browser
import Date
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Member exposing (Member)
import Message exposing (Event, GetMessagesResponse, Message(..), RoomEvent, getMessages, onlyMessageEvents)
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
    , room : Maybe Room
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
      , room = Nothing
      , error = Nothing
      }
    , Task.attempt GotRoom <| getInitialRoom config
    )


type Msg
    = GotRoom (Result Http.Error Room)
    | ViewMoreClicked
    | GotMessages (Result Http.Error GetMessagesResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.room ) of
        ( GotRoom (Ok room), _ ) ->
            ( { model | room = Just room }
            , Cmd.none
            )

        ( GotRoom (Err err), _ ) ->
            ( { model | error = Just <| Debug.toString err }
            , Cmd.none
            )

        ( ViewMoreClicked, Just room ) ->
            ( model
            , Task.attempt GotMessages <|
                getMessages
                    { homeserverUrl = model.config.defaultHomeserverUrl
                    , accessToken = room.accessToken
                    , roomId = room.roomId
                    , from = room.end
                    }
            )

        ( ViewMoreClicked, _ ) ->
            ( { model | error = Just "Can't fetch messages: no connection to homeserver" }
            , Cmd.none
            )

        ( GotMessages (Ok newMsgs), Just room ) ->
            ( { model | room = Just <| mergeNewMessages room newMsgs }
            , Cmd.none
            )

        ( GotMessages (Err httpErr), Just room ) ->
            ( { model | error = Just <| Debug.toString httpErr }
            , Cmd.none
            )

        ( GotMessages newMsgs, _ ) ->
            ( { model | error = Just "Unexpected state: got message response without a room" }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div [ class "octopus-container" ] <|
        [ -- view errors
          h5 [] <|
            case model.error of
                Nothing ->
                    []

                Just errmsg ->
                    [ text <| "ERROR: " ++ errmsg ]
        , -- show MessageEvents
          case model.room of
            Nothing ->
                p [] [ text "Getting comments..." ]

            Just room ->
                div []
                    [ viewRoomEvents
                        model.config.defaultHomeserverUrl
                        room.members
                        room.events
                    , viewMoreButton
                    ]
        ]


viewRoomEvents : String -> Dict String Member -> List RoomEvent -> Html Msg
viewRoomEvents defaultHomeserverUrl members roomEvents =
    div [] <|
        List.map
            (viewMessageEvent defaultHomeserverUrl members)
            (onlyMessageEvents roomEvents)


toUtcString : Int -> String
toUtcString timestamp =
    let
        time =
            Time.millisToPosix timestamp

        dateStr =
            String.fromInt (Time.toYear Time.utc time)
                ++ "-"
                ++ String.fromInt (Date.monthToNumber <| Time.toMonth Time.utc time)
                ++ "-"
                ++ String.fromInt (Time.toDay Time.utc time)

        timeStr =
            String.fromInt (Time.toHour Time.utc time)
                ++ ":"
                ++ String.fromInt (Time.toMinute Time.utc time)
                ++ ":"
                ++ String.fromInt (Time.toSecond Time.utc time)
                ++ " (UTC)"
    in
    dateStr ++ " " ++ timeStr


viewMessageEvent : String -> Dict String Member -> Event Message -> Html Msg
viewMessageEvent defaultHomeserverUrl members messageEvent =
    let
        member : Maybe Member
        member =
            Dict.get messageEvent.sender members

        displayname : String
        displayname =
            member
                |> Maybe.map (\m -> Maybe.withDefault "" m.displayname)
                |> Maybe.withDefault messageEvent.sender

        avatarUrl : Maybe String
        avatarUrl =
            member
                |> Maybe.map
                    (\m ->
                        case m.avatarUrl of
                            Just mxcUrl ->
                                mxcToHttp defaultHomeserverUrl mxcUrl

                            Nothing ->
                                Nothing
                    )
                |> Maybe.withDefault Nothing

        matrixDotToUrl : String
        matrixDotToUrl =
            "https://matrix.to/#/" ++ messageEvent.sender

        timeStr : String
        timeStr =
            toUtcString messageEvent.originServerTs

        textBody =
            case messageEvent.content of
                Text textMessage ->
                    textMessage.body

                _ ->
                    "unsupported message event"
    in
    div [ class "octopus-comment" ]
        -- image
        [ div [ class "octopus-comment-avatar" ]
            [ img [ src <| Maybe.withDefault "" avatarUrl ] [] ]
        , div [ class "octopus-comment-content" ]
            -- name and time
            [ div [ class "octopus-comment-header" ]
                [ p [ class "octopus-comment-displayname" ] [ a [ href matrixDotToUrl ] [ text displayname ] ]
                , p [ class "octopus-comment-time" ] [ text timeStr ]
                ]

            -- body
            , div [ class "octopus-comment-body" ]
                [ p [] [ text textBody ] ]
            ]
        ]


viewMoreButton : Html Msg
viewMoreButton =
    button
        [ onClick ViewMoreClicked ]
        [ text "View more" ]
