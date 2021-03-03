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

import Accessibility exposing (Html, a, b, div, i, img, p, text)
import ApiUtils exposing (httpFromMxc, thumbnailFromMxc)
import Dict exposing (Dict)
import Duration
import FormattedText exposing (FormattedText(..), decodeFormattedText, viewFormattedText)
import Html.Attributes exposing (class, height, href, src, width)
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
    , url : String
    , info : Maybe ImageInfo
    }


type alias ImageInfo_ a =
    { a
        | w : Int
        , h : Int
    }


type alias ThumbnailInfo =
    ImageInfo_ {}


type alias ImageInfo =
    ImageInfo_
        { thumbnail_url : Maybe String
        , thumbnail_info : Maybe ThumbnailInfo
        }


decodeImage : JD.Decoder ImageData
decodeImage =
    JD.map3 ImageData
        (JD.field "body" JD.string)
        (JD.field "url" JD.string)
        (JD.maybe <| JD.field "info" decodeImageInfo)


decodeImageInfo : JD.Decoder ImageInfo
decodeImageInfo =
    JD.map4
        (\w h tnurl tninfo ->
            { w = w
            , h = h
            , thumbnail_url = tnurl
            , thumbnail_info = tninfo
            }
        )
        (JD.field "w" JD.int)
        (JD.field "h" JD.int)
        (JD.maybe <| JD.field "thumbnail_url" JD.string)
        (JD.maybe <| JD.field "thumbnail_info" decodeThumbnailInfo)


decodeThumbnailInfo : JD.Decoder ThumbnailInfo
decodeThumbnailInfo =
    JD.map2
        (\w h -> { w = w, h = h })
        (JD.field "w" JD.int)
        (JD.field "h" JD.int)


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
                mainImage : ( Maybe String, Maybe ImageInfo )
                mainImage =
                    ( httpFromMxc homeserverUrl image.url
                    , image.info
                    )

                thumbnail : Maybe ( String, ThumbnailInfo )
                thumbnail =
                    case image.info of
                        Nothing ->
                            Nothing

                        Just info ->
                            case
                                ( Maybe.map (httpFromMxc homeserverUrl) info.thumbnail_url
                                    |> Maybe.withDefault Nothing
                                , info.thumbnail_info
                                )
                            of
                                ( Just url, Just tninfo ) ->
                                    Just ( url, tninfo )

                                _ ->
                                    Nothing
            in
            case ( mainImage, thumbnail ) of
                ( ( Just mainUrl, _ ), Just ( tnurl, tninfo ) ) ->
                    -- valid image url and thumbnail
                    -- render thumbnail
                    a [ href mainUrl ]
                        [ img image.body
                            [ class "cactus-message-image"
                            , src tnurl
                            , width tninfo.w
                            , height tninfo.h
                            ]
                        ]

                ( ( Just mainUrl, Just info ), Nothing ) ->
                    -- valid url, no thumbnail, full main image metadata
                    -- just show main imagae
                    a [ href mainUrl ]
                        [ img image.body
                            [ class "cactus-message-image"
                            , src mainUrl
                            , width info.w
                            , height info.h
                            ]
                        ]

                ( ( Just mainUrl, Nothing ), Nothing ) ->
                    -- valid url, nothing else
                    a [ href mainUrl ]
                        [ img image.body
                            [ class "cactus-message-image"
                            , src mainUrl
                            ]
                        ]

                _ ->
                    p [] [ i [] [ text "Error: Could not render image" ] ]

        _ ->
            -- TODO: this shouldn't be a thing
            p [] [ text "unsupported message event" ]
