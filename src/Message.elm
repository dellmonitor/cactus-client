module Message exposing
    ( Event
    , GetMessagesResponse
    , Message(..)
    , RoomEvent(..)
    , decodeMessages
    , formatTimeAsIsoUtcString
    , formatTimeAsUtcString
    , messageEvents
    , timeSinceText
    , viewMessageEvent
    )

import Accessibility exposing (Html, a, div, img, p, text)
import ApiUtils exposing (thumbnailFromMxc)
import DateFormat
import Dict exposing (Dict)
import Duration
import Html.Attributes exposing (class, datetime, href, src, title)
import Json.Decode as JD
import Maybe.Extra
import Member exposing (Member)
import Message.Audio exposing (AudioData, decodeAudio, viewAudio)
import Message.File exposing (FileData, decodeFile, viewFile)
import Message.FormattedText exposing (FormattedText(..), decodeFormattedText, viewFormattedText)
import Message.Image exposing (ImageData, decodeImage, viewImage)
import Message.Video exposing (VideoData, decodeVideo, viewVideo)
import Time


type RoomEvent
    = MessageEvent (Event Message)
    | StateEvent (Event State)
    | UnsupportedEvent (Event ())


type alias Event a =
    { eventType : String
    , content : a
    , sender : String
    , originServerTs : Time.Posix
    }


type State
    = Created String


type Message
    = Text FormattedText
    | Emote FormattedText
    | Notice FormattedText
    | Image ImageData
    | File FileData
    | Video VideoData
    | Audio AudioData
    | Location
    | UnsupportedMessageType


messageEvents : List RoomEvent -> List (Event Message)
messageEvents roomEvents =
    -- filter room events for message events
    List.foldl
        (\roomEvent msgs ->
            case roomEvent of
                MessageEvent msg ->
                    msg :: msgs

                _ ->
                    msgs
        )
        []
        roomEvents


type alias GetMessagesResponse =
    { start : String
    , end : String
    , chunk : List RoomEvent
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
    JD.oneOf
        [ -- switch on room event type,
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

                        "m.room.create" ->
                            makeRoomEvent
                                (\t msg s ots ->
                                    StateEvent
                                        { eventType = t
                                        , content = msg
                                        , sender = s
                                        , originServerTs = ots
                                        }
                                )
                                decodeCreate

                        _ ->
                            JD.fail ("Unsupported event type: " ++ eventType)
                )

        -- on failure: return unsupported event
        , makeRoomEvent
            (\t msg s ots ->
                UnsupportedEvent
                    { eventType = t
                    , content = msg
                    , sender = s
                    , originServerTs = ots
                    }
            )
            (JD.succeed ())
        ]


decodeCreate : JD.Decoder State
decodeCreate =
    JD.map Created <| JD.field "creator" JD.string


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
                        JD.map Notice decodeFormattedText

                    "m.image" ->
                        JD.map Image decodeImage

                    "m.file" ->
                        JD.map File decodeFile

                    "m.audio" ->
                        JD.map Audio decodeAudio

                    "m.video" ->
                        JD.map Video decodeVideo

                    "m.location" ->
                        JD.succeed UnsupportedMessageType

                    _ ->
                        JD.succeed UnsupportedMessageType
            )



-- VIEW


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
    if Duration.inSeconds diff > 0 then
        (String.fromInt <| floor <| unitfun diff) ++ " " ++ unitname ++ " ago"

    else
        "just now"


formatTimeAsUtcString : Time.Posix -> String
formatTimeAsUtcString time =
    let
        -- Format: Sun Mar 14 16:23:15 2021 UTC
        timeFormatter : Time.Zone -> Time.Posix -> String
        timeFormatter =
            DateFormat.format
                [ DateFormat.dayOfWeekNameAbbreviated
                , DateFormat.text " "
                , DateFormat.monthNameAbbreviated
                , DateFormat.text " "
                , DateFormat.dayOfMonthFixed
                , DateFormat.text " "
                , DateFormat.hourMilitaryFixed
                , DateFormat.text ":"
                , DateFormat.minuteFixed
                , DateFormat.text ":"
                , DateFormat.secondFixed
                , DateFormat.text " "
                , DateFormat.yearNumber
                , DateFormat.text " UTC"
                ]
    in
    timeFormatter Time.utc time


formatTimeAsIsoUtcString : Time.Posix -> String
formatTimeAsIsoUtcString time =
    let
        -- Format: 2020-12-03T02:05:16+00:00
        timeFormatter : Time.Zone -> Time.Posix -> String
        timeFormatter =
            DateFormat.format
                [ DateFormat.yearNumber
                , DateFormat.text "-"
                , DateFormat.monthFixed
                , DateFormat.text "-"
                , DateFormat.dayOfMonthFixed
                , DateFormat.text "T"
                , DateFormat.hourMilitaryFixed
                , DateFormat.text ":"
                , DateFormat.minuteFixed
                , DateFormat.text ":"
                , DateFormat.secondFixed
                , DateFormat.text "+00:00"
                ]
    in
    timeFormatter Time.utc time


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

        timeUtc : String
        timeUtc =
            formatTimeAsUtcString messageEvent.originServerTs

        timeUtcIso : String
        timeUtcIso =
            formatTimeAsIsoUtcString messageEvent.originServerTs

        body : Html msg
        body =
            viewMessage defaultHomeserverUrl displayname messageEvent.content
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
                , Accessibility.time
                    [ class "cactus-comment-time", title timeUtc, datetime timeUtcIso ]
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
                |> Maybe.andThen .avatarUrl
                |> Maybe.map (thumbnailFromMxc homeserverUrl)
                |> Maybe.Extra.join
    in
    div [ class "cactus-comment-avatar" ] <|
        case avatarUrl of
            Just url ->
                [ img "user avatar image" [ src url ] ]

            Nothing ->
                [ p [] [ text "?" ] ]


viewMessage : String -> String -> Message -> Html msg
viewMessage homeserverUrl displayname message =
    case message of
        Text fmt ->
            div
                [ class "cactus-message-text" ]
                [ viewFormattedText homeserverUrl fmt ]

        Emote (Plain str) ->
            div
                [ class "cactus-message-emote" ]
                [ p [] [ text <| displayname ++ " " ++ str ] ]

        Emote fmt ->
            div
                [ class "cactus-message-text" ]
                [ viewFormattedText homeserverUrl fmt ]

        Notice fmt ->
            div
                [ class "cactus-message-text" ]
                [ viewFormattedText homeserverUrl fmt ]

        Image image ->
            viewImage homeserverUrl image

        File file ->
            viewFile homeserverUrl file

        Audio audio ->
            viewAudio homeserverUrl audio

        Video video ->
            viewVideo homeserverUrl video

        _ ->
            -- TODO: this shouldn't be a thing
            p [] [ text "unsupported message event" ]
