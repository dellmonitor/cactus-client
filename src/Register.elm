module Register exposing (RegisterResponse, registerGuest)

import ApiUtils exposing (apiRequest, clientEndpoint)
import Http
import Json.Decode as JD
import Task exposing (Task)
import Url.Builder


type alias RegisterResponse =
    { userId : String
    , serverName : String
    , accessToken : String
    }


registerGuest : String -> Task Http.Error RegisterResponse
registerGuest homeserverUrl =
    apiRequest
        { method = "POST"
        , url = clientEndpoint homeserverUrl [ "register" ] [ Url.Builder.string "kind" "guest" ]
        , responseDecoder = decodeRegisterResponse
        , accessToken = Nothing
        , body = Http.stringBody "application/json" "{}"
        }


decodeRegisterResponse : JD.Decoder RegisterResponse
decodeRegisterResponse =
    let
        serverNameFromUserId : String -> Maybe String
        serverNameFromUserId userId =
            userId
                |> String.split ":"
                |> List.drop 1
                |> List.head
    in
    JD.map3 RegisterResponse
        -- userId
        (JD.field "user_id" JD.string)
        -- serverName
        (JD.field "user_id" JD.string
            |> JD.andThen
                (\userId ->
                    case serverNameFromUserId userId of
                        Nothing ->
                            JD.fail <| "Could not parse serverName from userId: " ++ userId

                        Just serverName ->
                            JD.succeed serverName
                )
        )
        -- accessToken
        (JD.field "access_token" JD.string)
