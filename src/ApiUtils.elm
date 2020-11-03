module ApiUtils exposing (apiRequest, clientEndpoint, httpFromMxc, makeRoomAlias, matrixDotToUrl, mediaEndpoint, serverNameFromId, thumbnailFromMxc, unauthenticatedRequest)

import Http
import Json.Decode as JD
import Task exposing (Task)
import Url exposing (percentEncode)
import Url.Builder exposing (QueryParameter, crossOrigin)


{-| Make a matrix room alias given a sitename, a unique id for the comments
section, and a matrix homeserver servername.

    makeRoomAlias { siteName = "myblog", uniqueId = "october-blogpost", serverName = "matrix.example.com" }
        == "#comments_myblog.com_october-blogpost:matrix.example.com"

-}
makeRoomAlias : { a | siteName : String, uniqueId : String, serverName : String } -> String
makeRoomAlias { siteName, uniqueId, serverName } =
    "#comments_" ++ siteName ++ "_" ++ uniqueId ++ ":" ++ serverName



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


mxcServerName : String -> Maybe String
mxcServerName mxcUrl =
    mxcUrl
        |> String.dropLeft 6
        |> String.split "/"
        |> List.head


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



-- HTTP REQUESTS


{-| handle the JSON response of a HTTP Request
Flatten HTTP and JSON errors.
-}
handleJsonResponse : JD.Decoder a -> Http.Response String -> Result Http.Error a
handleJsonResponse decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.BadStatus_ { statusCode } _ ->
            Err (Http.BadStatus statusCode)

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.GoodStatus_ _ body ->
            case JD.decodeString decoder body of
                Err _ ->
                    Err (Http.BadBody body)

                Ok result ->
                    Ok result


{-| Make an optionally authenticated requests to a Matrix homeserver.
-}
apiRequest :
    { method : String
    , url : String
    , accessToken : Maybe String
    , responseDecoder : JD.Decoder a
    , body : Http.Body
    }
    -> Task Http.Error a
apiRequest { method, url, accessToken, responseDecoder, body } =
    Http.task
        { method = method
        , headers =
            accessToken
                |> Maybe.map (\at -> [ Http.header "Authorization" <| "Bearer " ++ at ])
                |> Maybe.withDefault []
        , url = url
        , body = body
        , resolver = Http.stringResolver <| handleJsonResponse responseDecoder
        , timeout = Nothing
        }


{-| Make unauthenticated requests to a Matrix API
-}
unauthenticatedRequest : { method : String, url : String, body : Http.Body, responseDecoder : JD.Decoder a } -> Task Http.Error a
unauthenticatedRequest { method, url, body, responseDecoder } =
    apiRequest
        { method = method
        , url = url
        , body = body
        , responseDecoder = responseDecoder
        , accessToken = Nothing
        }
