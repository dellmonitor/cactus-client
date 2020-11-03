module Member exposing (Member, getJoinedMembers)

import Dict exposing (Dict)
import Http
import Json.Decode as JD
import Session exposing (Session, authenticatedRequest)
import Task exposing (Task)


type alias Member =
    { displayname : Maybe String
    , avatarUrl : Maybe String
    , userId : String
    }


getJoinedMembers : Session -> String -> Task Http.Error (Dict String Member)
getJoinedMembers session roomId =
    authenticatedRequest
        session
        { method = "GET"
        , path = [ "rooms", roomId, "members" ]
        , params = []
        , responseDecoder = decodeMemberResponse
        , body = Http.emptyBody
        }


decodeMemberResponse : JD.Decoder (Dict String Member)
decodeMemberResponse =
    (JD.field "chunk" <| JD.list decodeMemberEvent)
        |> JD.andThen
            (\roomMembers ->
                roomMembers
                    |> List.map (\rm -> ( rm.userId, rm ))
                    |> Dict.fromList
                    |> JD.succeed
            )


decodeMemberEvent : JD.Decoder Member
decodeMemberEvent =
    (JD.field "content" <| decodeMemberContent)
        |> JD.andThen
            (\content ->
                JD.field "state_key" JD.string
                    |> JD.andThen (\uid -> JD.succeed <| Member content.displayname content.avatarUrl uid)
            )


decodeMemberContent : JD.Decoder { displayname : Maybe String, avatarUrl : Maybe String }
decodeMemberContent =
    JD.map2 (\dn au -> { displayname = dn, avatarUrl = au })
        (JD.maybe <| JD.field "displayname" JD.string)
        (JD.maybe <| JD.field "avatar_url" JD.string)
