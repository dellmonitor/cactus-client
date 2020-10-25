module FormattedText exposing (FormattedText(..), decodeFormattedText, viewFormattedText)

import ApiUtils exposing (httpFromMxc)
import Html exposing (..)
import Html.Parser
import Html.Parser.Util
import Json.Decode as JD



{-
   This module supports message event bodies that optionally have the `formatted_body` and `format` fields.
   Currently the only supported format is org.matrix.custom.html
   The main responsibility of this module is to parse HTML to avoid XSS.
-}


type FormattedText
    = Plain String
    | Html (List Html.Parser.Node)



-- DECODERS


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
                        -- XXX: the html is not sanitized at this point
                        --      sanitizaiton happens when viewing
                        JD.succeed (Html nodes)
            )



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


cleanHtmlNode : String -> Html.Parser.Node -> Html.Parser.Node
cleanHtmlNode homeserverUrl node =
    -- TODO: observe max depth as recommended by C/S spec
    case node of
        Html.Parser.Text _ ->
            -- raw text gets to stay
            node

        Html.Parser.Comment str ->
            -- keep comments also
            node

        Html.Parser.Element tag attrs children ->
            -- if tag in whitelist, clean the attributes and children
            if List.member tag tagWhitelist then
                Html.Parser.Element
                    tag
                    (cleanAttributes homeserverUrl tag attrs)
                    (List.map (cleanHtmlNode homeserverUrl) children)

            else
                -- element not in whitelist - remove it
                Html.Parser.Text ""


cleanAttributes : String -> String -> List ( String, String ) -> List ( String, String )
cleanAttributes homeserverUrl tag attrs =
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
            imgAttributes homeserverUrl attrs

        "ol" ->
            []

        "code" ->
            []

        _ ->
            []


{-| Keep only the color attributes allowed by the client/server api spec
and translate the color attributes to inline css

    colorAttributes [("data-mx-color", "#ffc0de), ("foo", "bar"), ("data-mx-bg-color", "#c0deff")] == [("style", "color: #ffc0de), ("style", "background: #c0deff")]

-}
colorAttributes : List ( String, String ) -> List ( String, String )
colorAttributes attrs =
    List.foldl
        (\attr list ->
            case attr of
                ( "data-mx-color", colorStr ) ->
                    -- XXX: this is vulnurable to css injection
                    -- TODO: fix this shit
                    ( "style", "color: " ++ colorStr ) :: list

                ( "data-mx-bg-color", colorStr ) ->
                    -- XXX: this is vulnurable to css injection
                    -- TODO: fix this shit
                    ( "style", "background: " ++ colorStr ) :: list

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


imgAttributes : String -> List ( String, String ) -> List ( String, String )
imgAttributes homeserverUrl attrs =
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
                    -- only keep if mxc parsing succeeds
                    httpFromMxc homeserverUrl srcStr
                        |> Maybe.map (\url -> ( "src", url ) :: list)
                        |> Maybe.withDefault list

                _ ->
                    list
        )
        []
        attrs



-- VIEW


viewFormattedText : String -> FormattedText -> Html msg
viewFormattedText homeserverUrl fmt =
    case fmt of
        Plain str ->
            p [] [ text str ]

        Html nodes ->
            div [] (List.map (cleanHtmlNode homeserverUrl) nodes |> Html.Parser.Util.toVirtualDom)
