module Message exposing (Event, GetMessagesResponse, Message(..), RoomEvent(..), getMessages, onlyMessageEvents, viewMessageEvent)

import ApiUtils exposing (apiRequest, clientEndpoint, thumbnailFromMxc)
import Date
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Parser
import Html.Parser.Util
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
    , originServerTs : Int
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


type FormattedText
    = Plain String
    | Html (List Html.Parser.Node)


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
        makeRoomEvent : (String -> a -> String -> Int -> RoomEvent) -> JD.Decoder a -> JD.Decoder RoomEvent
        makeRoomEvent constructor contentDecoder =
            JD.map4
                constructor
                (JD.field "type" JD.string)
                (JD.field "content" contentDecoder)
                (JD.field "sender" JD.string)
                (JD.field "origin_server_ts" JD.int)
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
                        decodeTextMessage

                    "m.emote" ->
                        JD.succeed (Emote <| Plain "TODO: Implement me")

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


decodeFormattedText : JD.Decoder FormattedText
decodeFormattedText =
    (JD.maybe <| JD.field "format" JD.string)
        |> JD.andThen
            (\f ->
                case f of
                    Just "org.matrix.custom.html" ->
                        decodeTextHtml

                    _ ->
                        decodeTextPlain
            )


decodeTextPlain : JD.Decoder FormattedText
decodeTextPlain =
    JD.field "body" JD.string
        |> JD.map Plain


decodeTextHtml : JD.Decoder FormattedText
decodeTextHtml =
    JD.field "formatted_body" JD.string
        |> JD.map Html.Parser.run
        |> JD.andThen
            (\html ->
                case html of
                    Err _ ->
                        -- fall back to plain body
                        decodeTextPlain

                    Ok nodes ->
                        -- successful html parse
                        -- TODO: clean up HTML
                        JD.succeed <| Html nodes
            )


decodeTextMessage : JD.Decoder Message
decodeTextMessage =
    JD.map Text <| decodeFormattedText



{- SANITIZE
   Clean parsed HTML, keeping only whitelisted nodes and attributes as per
   the Client-Server API spec r.0.6.1 - 13.2.1.7 m.room.message msgtypes.
   https://matrix.org/docs/spec/client_server/r0.6.1#m-room-message-msgtypes
-}


tagWhitelist =
    [ "font"
    , "del"
    , "h1"
    , "h2"
    , "h3"
    , "h4"
    , "h5"
    , "h6"
    , "blockquote"
    , "p"
    , "a"
    , "ul"
    , "ol"
    , "sup"
    , "sub"
    , "li"
    , "b"
    , "i"
    , "u"
    , "strong"
    , "em"
    , "strike"
    , "code"
    , "hr"
    , "br"
    , "div"
    , "table"
    , "thead"
    , "tbody"
    , "tr"
    , "th"
    , "td"
    , "caption"
    , "pre"
    , "span"
    , "img"
    ]


cleanHtmlNode : Html.Parser.Node -> Html.Parser.Node
cleanHtmlNode node =
    -- XXX: this is not complete
    case node of
        Html.Parser.Text _ ->
            node

        Html.Parser.Element tag attrs children ->
            if List.member tag tagWhitelist then
                Html.Parser.Element
                    tag
                    (cleanAttributes tag attrs)
                    (List.map cleanHtmlNode children)

            else
                -- element not in whitelist - leave empty
                Html.Parser.Text ""

        _ ->
            -- TODO: last case
            node


cleanAttributes : String -> List ( String, String ) -> List ( String, String )
cleanAttributes tag attrs =
    -- keep omly the attributes whitelisted by the client/server api spec
    -- and transform them as per the spec
    case tag of
        "font" ->
            colorAttributes attrs

        "span" ->
            colorAttributes attrs

        "a" ->
            -- "prevent target page from referencing the client's tab/window"
            ( "rel", "noopener" ) :: anchorAttributes attrs

        "img" ->
            []

        "ol" ->
            []

        "code" ->
            []

        _ ->
            []


colorAttributes : List ( String, String ) -> List ( String, String )
colorAttributes attrs =
    -- keep only the color attributes allowed by the client/server api spec
    -- and translate the color attributes to inline css
    List.foldl
        (\attr list ->
            case attr of
                ( "data-mx-color", colorStr ) ->
                    -- XXX: this is vulnurable to css injection
                    -- TODO: fix this shit
                    ( "style", "color: #" ++ colorStr ) :: list

                ( "data-mx-bg-color", colorStr ) ->
                    -- XXX: this is vulnurable to css injection
                    -- TODO: fix this shit
                    ( "style", "background: #" ++ colorStr ) :: list

                _ ->
                    -- discard all other attributes
                    list
        )
        []
        attrs


anchorAttributes : List ( String, String ) -> List ( String, String )
anchorAttributes attrs =
    List.foldl
        (\attr list ->
            case attr of
                ( "name", nameStr ) ->
                    ( "name", nameStr ) :: list

                ( "target", targetStr ) ->
                    ( "target", targetStr ) :: list

                ( "href", hrefStr ) ->
                    let
                        validSchemas =
                            [ "https"
                            , "http"
                            , "ftp"
                            , "mailto"
                            , "magnet"
                            ]

                        hrefClean : Bool
                        hrefClean =
                            List.head (String.split ":" hrefStr)
                                |> Maybe.map (\schema -> List.member schema validSchemas)
                                |> Maybe.withDefault False
                    in
                    if hrefClean then
                        ( "href", hrefStr ) :: list

                    else
                        list

                _ ->
                    list
        )
        []
        attrs


imgAttributes : List ( String, String ) -> List ( String, String )
imgAttributes attrs =
    List.foldl
        (\attr list ->
            case attr of
                ( "width", _ ) ->
                    attr :: list

                ( "height", _ ) ->
                    attr :: list

                ( "alt", _ ) ->
                    attr :: list

                ( "title", _ ) ->
                    attr :: list

                ( "src", srcStr ) ->
                    -- TODO: get a function that can change to media url
                    ( "src", "TODO" ) :: list

                _ ->
                    list
        )
        []
        attrs



-- VIEW


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


viewMessageEvent : String -> Dict String Member -> Event Message -> Html msg
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
                                thumbnailFromMxc defaultHomeserverUrl mxcUrl

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

        body : Html msg
        body =
            viewMessage messageEvent.content
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
                [ body ]
            ]
        ]


viewMessage : Message -> Html msg
viewMessage message =
    case message of
        Text (Plain str) ->
            p [] [ text str ]

        Text (Html html) ->
            div [] <| Html.Parser.Util.toVirtualDom html

        _ ->
            -- TODO: this shouldn't be a thing
            p [] [ text "unsupported message event" ]
