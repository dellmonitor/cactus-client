module Message exposing (Event, GetMessagesResponse, Message(..), RoomEvent(..), getMessages, onlyMessageEvents, timeSinceText, viewMessageEvent)

import ApiUtils exposing (apiRequest, clientEndpoint, httpFromMxc, thumbnailFromMxc)
import Date
import Dict exposing (Dict)
import Duration
import FormattedText exposing (FormattedText(..), decodeFormattedText, viewFormattedText)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as JD
import Member exposing (Member)
import Task exposing (Task)
import Time
import Url.Builder


type alias Event a =
    { eventType : String
    , content : a
    , sender : String
    , originServerTs : Time.Posix
    }


type RoomEvent
    = MessageEvent (Event Message)
    | UnsupportedEvent (Event ())


type Message
    = Text FormattedText
    | Emote FormattedText
    | Notice
    | Image
    | File
    | Audio
    | Location
    | Video
    | UnsupportedMessageType


onlyMessageEvents : List RoomEvent -> List (Event Message)
onlyMessageEvents roomEvents =
    -- filter room events for message events
    List.foldl
        (\roomEvent messageEvents ->
            case roomEvent of
                MessageEvent msg ->
                    msg :: messageEvents

                _ ->
                    messageEvents
        )
        []
        roomEvents


type alias GetMessagesResponse =
    { start : String
    , end : String
    , chunk : List RoomEvent
    }


getMessages :
    { homeserverUrl : String, accessToken : String, roomId : String, from : String }
    -> Task Http.Error GetMessagesResponse
getMessages { homeserverUrl, accessToken, roomId, from } =
    apiRequest
        { method = "GET"
        , url =
            clientEndpoint homeserverUrl
                [ "rooms", roomId, "messages" ]
                [ Url.Builder.string "dir" "b"
                , Url.Builder.string "from" <| from
                ]
        , accessToken = Just accessToken
        , responseDecoder = decodeMessages
        , body = Http.emptyBody
        }


decodeMessages : JD.Decoder { start : String, end : String, chunk : List RoomEvent }
decodeMessages =
    JD.map3
        (\start end chunk -> { start = start, end = end, chunk = chunk })
        (JD.field "start" JD.string)
        (JD.field "end" JD.string)
        (JD.field "chunk" <| JD.list decodeRoomEvent)


decodeRoomEvent : JD.Decoder RoomEvent
decodeRoomEvent =
    let
        makeRoomEvent : (String -> a -> String -> Time.Posix -> RoomEvent) -> JD.Decoder a -> JD.Decoder RoomEvent
        makeRoomEvent constructor contentDecoder =
            JD.map4
                constructor
                (JD.field "type" JD.string)
                (JD.field "content" contentDecoder)
                (JD.field "sender" JD.string)
                (JD.field "origin_server_ts" JD.int |> JD.map Time.millisToPosix)
    in
    -- switch on room event type,
    JD.field "type" JD.string
        |> JD.andThen
            (\eventType ->
                case eventType of
                    "m.room.message" ->
                        makeRoomEvent
                            (\t msg s ots ->
                                MessageEvent
                                    { eventType = t
                                    , content = msg
                                    , sender = s
                                    , originServerTs = ots
                                    }
                            )
                            decodeMessage

                    _ ->
                        makeRoomEvent
                            (\t msg s ots ->
                                UnsupportedEvent
                                    { eventType = t
                                    , content = msg
                                    , sender = s
                                    , originServerTs = ots
                                    }
                            )
                            (JD.succeed ())
            )


decodeMessage : JD.Decoder Message
decodeMessage =
    JD.field "msgtype" JD.string
        |> JD.andThen
            (\mt ->
                case mt of
                    "m.text" ->
                        JD.map Text decodeFormattedText

                    "m.emote" ->
                        JD.map Emote decodeFormattedText

                    "m.notice" ->
                        JD.succeed Notice

                    "m.image" ->
                        JD.succeed Image

                    "m.file" ->
                        JD.succeed File

                    "m.audio" ->
                        JD.succeed Audio

                    "m.location" ->
                        JD.succeed Location

                    "m.video" ->
                        JD.succeed Video

                    _ ->
                        JD.succeed UnsupportedMessageType
            )



-- VIEW


{-| toUtcString gives a formatted string showing the posix time given as a
YYYY-MM-DD HH:MM:SS string.

    toUtcString 0 == "1970-01-01 00:00:00 (UTC)"

-}
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


{-| timeSinceText gives a natural language string that says how long ago the
comment was posted.

    timeSince (Time.millisToPosix 4000) (Time.millisToPosix 1000) == "3 seconds ago"

-}
timeSinceText : Time.Posix -> Time.Posix -> String
timeSinceText now then_ =
    let
        diff =
            Duration.from then_ now

        allTimeUnits : List ( String, Duration.Duration -> Float )
        allTimeUnits =
            [ ( "years", Duration.inJulianYears )
            , ( "months", Duration.inJulianYears >> (*) 12 )
            , ( "weeks", Duration.inWeeks )
            , ( "days", Duration.inDays )
            , ( "hours", Duration.inHours )
            , ( "minutes", Duration.inMinutes )
            , ( "seconds", Duration.inSeconds )
            ]

        biggestUnitGreaterThanOne : List ( String, Duration.Duration -> Float ) -> ( String, Duration.Duration -> Float )
        biggestUnitGreaterThanOne timeunits =
            -- expects the unit list to be sorted by size
            case timeunits of
                ( name, unit ) :: rest ->
                    if unit diff > 1 then
                        ( name, unit )

                    else
                        biggestUnitGreaterThanOne rest

                [] ->
                    ( "seconds", Duration.inSeconds )

        ( unitname, unitfun ) =
            biggestUnitGreaterThanOne allTimeUnits
    in
    (String.fromInt <| floor <| unitfun diff) ++ " " ++ unitname ++ " ago"


viewMessageEvent : String -> Time.Posix -> Dict String Member -> Event Message -> Html msg
viewMessageEvent defaultHomeserverUrl time members messageEvent =
    let
        member : Maybe Member
        member =
            Dict.get messageEvent.sender members

        displayname : String
        displayname =
            member
                |> Maybe.map (\m -> Maybe.withDefault messageEvent.sender m.displayname)
                |> Maybe.withDefault messageEvent.sender

        matrixDotToUrl : String
        matrixDotToUrl =
            "https://matrix.to/#/" ++ messageEvent.sender

        timeStr : String
        timeStr =
            timeSinceText time messageEvent.originServerTs

        body : Html msg
        body =
            viewMessage defaultHomeserverUrl messageEvent.content
    in
    div [ class "cactus-comment" ]
        [ -- avatar image
          viewAvatar defaultHomeserverUrl member
        , div [ class "cactus-comment-content" ]
            -- name and time
            [ div [ class "cactus-comment-header" ]
                [ p
                    [ class "cactus-comment-displayname" ]
                    [ a [ href matrixDotToUrl ] [ text displayname ] ]
                , p
                    [ class "cactus-comment-time" ]
                    [ text timeStr ]
                ]
            , --  body
              div [ class "cactus-comment-body" ] [ body ]
            ]
        ]


viewAvatar : String -> Maybe Member -> Html msg
viewAvatar homeserverUrl member =
    let
        avatarUrl : Maybe String
        avatarUrl =
            member
                |> Maybe.map
                    (\m ->
                        case m.avatarUrl of
                            Just mxcUrl ->
                                thumbnailFromMxc homeserverUrl mxcUrl

                            Nothing ->
                                Nothing
                    )
                |> Maybe.withDefault Nothing
    in
    div [ class "cactus-comment-avatar" ] <|
        case avatarUrl of
            Just url ->
                [ img [ src url ] [] ]

            Nothing ->
                [ p [] [ text "?" ] ]


viewMessage : String -> Message -> Html msg
viewMessage homeserverUrl message =
    case message of
        Text fmt ->
            div
                [ class "cactus-message-text" ]
                [ viewFormattedText homeserverUrl fmt ]

        Emote (Plain str) ->
            div
                [ class "cactus-message-emote" ]
                [ p [] [ text <| "Asbjørn " ++ str ] ]

        Emote fmt ->
            div
                [ class "cactus-message-text" ]
                [ viewFormattedText homeserverUrl fmt ]

        _ ->
            -- TODO: this shouldn't be a thing
            p [] [ text "unsupported message event" ]
