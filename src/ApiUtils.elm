module ApiUtils exposing (clientEndpoint, httpFromMxc, makeRoomAlias, matrixDotToUrl, mediaEndpoint, serverNameFromId, thumbnailFromMxc)

import Parser exposing ((|.), (|=), Parser)
import Set
import Url exposing (percentEncode)
import Url.Builder exposing (QueryParameter, crossOrigin)


{-| Make a matrix room alias given a sitename, a unique id for the comments
section, and a matrix homeserver servername.

    makeRoomAlias { siteName = "myblog", commentSectionId = "october-blogpost", serverName = "matrix.example.com" }
        == "#comments_myblog.com_october-blogpost:matrix.example.com"

-}
makeRoomAlias : { a | siteName : String, commentSectionId : String, serverName : String } -> String
makeRoomAlias { siteName, commentSectionId, serverName } =
    "#comments_" ++ siteName ++ "_" ++ commentSectionId ++ ":" ++ serverName



-- ENDPOINT URLS


apiEndpoint : List String -> String -> List String -> List QueryParameter -> String
apiEndpoint pathPrefix homeserverUrl path params =
    crossOrigin
        homeserverUrl
        (List.map percentEncode <| pathPrefix ++ path)
        params


clientEndpoint : String -> List String -> List QueryParameter -> String
clientEndpoint =
    apiEndpoint [ "_matrix", "client", "r0" ]


mediaEndpoint : String -> List String -> List QueryParameter -> String
mediaEndpoint =
    apiEndpoint [ "_matrix", "media", "r0" ]


{-| Make a matrix.to link from a matrix resource identifier.
This link can be used to access the room from other clients.

    matrixDotToUrl "@asbjorn:olli.ng" == "https://matrix.to/#/%40asbjorn%3Aolli.ng"

    matrixDotToUrl "#roomAlias:matrix.org" == "https://matrix.to/#/%23roomAlias%3Amatrix.org"

-}
matrixDotToUrl : String -> String
matrixDotToUrl identifier =
    -- https://matrix.to/#/<identifier>
    crossOrigin
        "https://matrix.to"
        [ "#", percentEncode identifier ]
        []


{-| Get the server name from a matrix resource identifier by splitting an identifier on ':'

    serverNameFromId "@user:server.com" == Just "server.com"

    serverNameFromId "#room:server.com" == Just "server.com"

    serverNameFromId "foobar" == Nothing

-}
serverNameFromId : String -> Maybe String
serverNameFromId id =
    if String.contains ":" id then
        id
            |> String.split ":"
            |> (List.reverse >> List.head)

    else
        Nothing



-- MEDIA


{-| Parse a server name from an mxc:// url
Server name grammar found here:
<https://matrix.org/docs/spec/appendices#identifier-grammar>
-}
mxcServerName : String -> Maybe String
mxcServerName mxcUrl =
    let
        validChar : Char -> Bool
        validChar c =
            Char.isAlphaNum c || List.member c [ '.', '-', ':' ]

        parser : Parser String
        parser =
            -- this parser sloppily allows ports in the middle of the domain
            -- like "abcd:8448:wow". it's not ideal but ¯\_(ツ)_/¯
            Parser.succeed identity
                |. Parser.token "mxc://"
                |= Parser.variable
                    { start = validChar
                    , inner = validChar
                    , reserved = Set.empty
                    }
    in
    Parser.run parser mxcUrl
        |> Result.toMaybe


mxcMediaId : String -> Maybe String
mxcMediaId mxcUrl =
    mxcUrl
        |> String.split "/"
        |> (List.reverse >> List.head)


thumbnailFromMxc : String -> String -> Maybe String
thumbnailFromMxc homeserverUrl mxcUrl =
    let
        serverName =
            mxcServerName mxcUrl

        mediaId =
            mxcMediaId mxcUrl
    in
    Maybe.map2
        (\sn mid ->
            mediaEndpoint homeserverUrl
                [ "thumbnail", sn, mid ]
                [ Url.Builder.int "width" 32
                , Url.Builder.int "height" 32
                , Url.Builder.string "method" "crop"
                ]
        )
        serverName
        mediaId


httpFromMxc : String -> String -> Maybe String
httpFromMxc homeserverUrl mxcUrl =
    let
        serverName =
            mxcServerName mxcUrl

        mediaId =
            mxcMediaId mxcUrl
    in
    Maybe.map2
        (\sn mid -> mediaEndpoint homeserverUrl [ "download", sn, mid ] [])
        serverName
        mediaId
