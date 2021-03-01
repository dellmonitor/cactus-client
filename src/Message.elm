module Message exposing
    ( Event
    , GetMessagesResponse
    , Message(..)
    , RoomEvent(..)
    , decodeMessages
    , getMessages
    , messageEvents
    , timeSinceText
    , viewMessageEvent
    )

import Accessibility exposing (Html, a, b, div, img, p, text)
import ApiUtils exposing (httpFromMxc, thumbnailFromMxc)
import Dict exposing (Dict)
import Duration
import FormattedText exposing (FormattedText(..), decodeFormattedText, viewFormattedText)
import Html.Attributes exposing (class, href, src)
import Http
import Json.Decode as JD
import Maybe.Extra
import Member exposing (Member)
import Session exposing (Session, authenticatedRequest)
import Task exposing (Task)
import Time
import Url.Builder


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
    | File
    | Audio
    | Location
    | Video
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


getMessages : Session -> { roomId : String, dir : String, from : String } -> Task Session.Error GetMessagesResponse
getMessages session { roomId, dir, from } =
    authenticatedRequest
        session
        { method = "GET"
        , path = [ "rooms", roomId, "messages" ]
        , params =
            [ Url.Builder.string "dir" dir
            , Url.Builder.string "from" from
            ]
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


type alias ImageData =
    { body : String
    , info : Maybe ImageInfo
    , url : String
    }


decodeImage : JD.Decoder ImageData
decodeImage =
    JD.map3 ImageData
        (JD.field "body" JD.string)
        (JD.maybe <| JD.field "info" decodeImageInfo)
        (JD.field "url" JD.string)


type alias ImageInfo =
    { h : Int
    , w : Int
    , mimetype : String
    , thumbnail_url : Maybe String
    , thumbnail_info :
        Maybe
            { h : Int
            , w : Int
            , mimetype : String
            }
    }


decodeImageInfo : JD.Decoder ImageInfo
decodeImageInfo =
    JD.map5
        ImageInfo
        (JD.field "h" JD.int)
        (JD.field "w" JD.int)
        (JD.field "mimetype" JD.string)
        (JD.maybe <| JD.field "thumbnail_url" JD.string)
        (JD.maybe <|
            JD.field "thumbnail_info"
                (JD.map3
                    (\w h mime -> { w = w, h = h, mimetype = mime })
                    (JD.field "w" JD.int)
                    (JD.field "h" JD.int)
                    (JD.field "mimetype" JD.string)
                )
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
                        JD.map Notice decodeFormattedText

                    "m.image" ->
                        JD.map Image decodeImage

                    "m.file" ->
                        JD.succeed File

                    "m.audio" ->
                        JD.succeed Audio

                    "m.video" ->
                        JD.succeed Video

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



-- TODO
-- viewImage : String -> ImageData -> Html msg
-- viewImage homeserverUrl image =


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
            let
                imgUrl : Maybe String
                imgUrl =
                    httpFromMxc homeserverUrl image.url

                thumbnailUrl : Maybe String
                thumbnailUrl =
                    image.info
                        |> Maybe.map
                            (\info ->
                                info.thumbnail_url
                                    |> Maybe.map (httpFromMxc homeserverUrl)
                                    |> Maybe.withDefault Nothing
                            )
                        |> Maybe.withDefault Nothing
            in
            case ( imgUrl, thumbnailUrl ) of
                ( _, Just url ) ->
                    img image.body
                        [ class "cactus-message-image" ]

                ( Just url, _ ) ->
                    img image.body
                        [ class "cactus-message-image" ]

                ( Nothing, Nothing ) ->
                    p [] [ b [] [ text "unsupported message event" ] ]

        _ ->
            -- TODO: this shouldn't be a thing
            p [] [ text "unsupported message event" ]
