module ApiUtils exposing (apiRequest, clientEndpoint, httpFromMxc, matrixDotToUrl, mediaEndpoint, thumbnailFromMxc)

import Http
import Json.Decode as JD
import Task exposing (Task)
import Url exposing (percentEncode)
import Url.Builder exposing (QueryParameter, crossOrigin)



-- ENDPOINT URLS


apiEndpoint : List String -> String -> List String -> List QueryParameter -> String
apiEndpoint pathPrefix homeserverUrl path params =
    crossOrigin
        homeserverUrl
        (pathPrefix ++ List.map percentEncode path)
        params


clientEndpoint : String -> List String -> List QueryParameter -> String
clientEndpoint =
    apiEndpoint [ "_matrix", "client", "r0" ]


mediaEndpoint : String -> List String -> List QueryParameter -> String
mediaEndpoint =
    apiEndpoint [ "_matrix", "media", "r0" ]


matrixDotToUrl : String -> String
matrixDotToUrl identifier =
    -- https://matrix.to/#/<identifier>
    crossOrigin
        "https://matrix.to"
        [ "#", percentEncode identifier ]
        []



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
