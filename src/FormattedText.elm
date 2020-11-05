module FormattedText exposing (FormattedText(..), cleanHtmlNode, decodeFormattedText, viewFormattedText)

import Accessibility exposing (Html, div, p, text)
import ApiUtils exposing (httpFromMxc)
import Html.Parser
import Html.Parser.Util
import Json.Decode as JD
import Set exposing (Set)
import Tuple



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


tagWhitelist : Set String
tagWhitelist =
    Set.fromList
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


{-| Clean and transform parsed HTML.

Removes tags and attributes not in whitelist.
Transforms mxc urls to http urls, and color tags to css attributes.

-}
cleanHtmlNode : String -> Html.Parser.Node -> Html.Parser.Node
cleanHtmlNode homeserverUrl node =
    mapElement (cleanElement homeserverUrl) node


{-| Function to be mapped over HTML tree.

Clean a single element's attributes, based on the value of the tag.
Replace the element with empty raw text, if tag not in whitelist.

-}
cleanElement : String -> Html.Parser.Node -> Html.Parser.Node
cleanElement homeserverUrl node =
    case node of
        Html.Parser.Element tag attrs children ->
            -- if tag in whitelist, clean the attributes and children
            if Set.member tag tagWhitelist then
                Html.Parser.Element
                    tag
                    (cleanAttributes homeserverUrl tag attrs)
                    children

            else
                -- element not in whitelist - remove it
                Html.Parser.Text ""

        Html.Parser.Text str ->
            -- raw text gets to stay
            Html.Parser.Text str

        Html.Parser.Comment str ->
            -- keep comments also
            Html.Parser.Comment str


{-| Map a function over all Html.Parser.Element values in a Html.Parser.Node tree

To avoid memory issues on large input sizes, this function is implemented in a continuation-passing style.
Currently it also truncates the children of an Elment to 20, which fixes an undiscovered performance bug.

-}
mapElement : (Html.Parser.Node -> Html.Parser.Node) -> Html.Parser.Node -> Html.Parser.Node
mapElement f root =
    let
        mapElementHelp : Html.Parser.Node -> (Html.Parser.Node -> ret) -> ret
        mapElementHelp node c =
            case node of
                Html.Parser.Element tag attrs children ->
                    let
                        continuations : List ((Html.Parser.Node -> ret) -> ret)
                        continuations =
                            List.map mapElementHelp <|
                                -- TODO: fix this performance bug
                                --       somewhy it crashes on bigger input
                                --       (when fuzz testing)
                                List.take 20 children

                        finalContinuation : List Html.Parser.Node -> ret
                        finalContinuation chs =
                            f (Html.Parser.Element tag attrs chs)
                                |> c
                    in
                    continuationSequence continuations finalContinuation

                _ ->
                    -- dont do anything to comments and text
                    c node
    in
    mapElementHelp root identity


continuationSequence : List ((a -> ret) -> ret) -> (List a -> ret) -> ret
continuationSequence recursions finalContinuation =
    case recursions of
        [] ->
            finalContinuation []

        recurse :: recurses ->
            recurse
                (\r ->
                    continuationSequence recurses
                        (\rs ->
                            r :: rs |> finalContinuation
                        )
                )


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
            -- only keep "start"
            List.filter
                (Tuple.first >> (==) "start")
                attrs

        "code" ->
            List.filter
                (\( attr, val ) ->
                    (attr == "class")
                        && (String.left 9 val == "language-")
                )
                attrs

        _ ->
            []


{-| Keep only the color attributes allowed by the client/server api spec
and translate the color attributes to inline css

    colorAttributes [("data-mx-color", "#ffc0de), ("foo", "bar"), ("data-mx-bg-color", "#c0deff")]
    == [("style", "color: #ffc0de), ("style", "background: #c0deff")]

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
