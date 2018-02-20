module FirebaseDict exposing (..)

import FirebaseDict.FDict as FDict
import Json.Encode as JE
import Json.Decode as JD
import Time exposing (Time)


-- Firebase

import Firebase.Database
import Firebase.Database.Snapshot
import Firebase.Database.Reference
import Firebase.Database.Types


type alias Config m v =
    { path : String
    , encoder : v -> JE.Value
    , decoder : JD.Decoder v
    , get : m -> FDict.FDict v
    , set : m -> FDict.FDict v -> m
    }


type Msg
    = Time Time
    | Snapshot Firebase.Database.Types.Snapshot


update : (Msg -> msg) -> Msg -> m -> Config m v -> ( m, Cmd msg )
update tagger msg model config =
    case msg of
        Time time ->
            let
                _ =
                    Debug.log "Time : " msg
            in
                ( model, Cmd.none )

        Snapshot snapshot ->
            let
                value : Result String v
                value =
                    snapshot
                        |> Firebase.Database.Snapshot.value
                        |> JD.decodeValue config.decoder
                        |> Debug.log "update Snapshot"

                key =
                    Firebase.Database.Snapshot.key snapshot

                insert m v =
                    case key of
                        Nothing ->
                            m

                        Just str ->
                            config.get m
                                |> FDict.insert str v
                                |> config.set m
            in
                case value of
                    Err str ->
                        ( model, Cmd.none )

                    Ok val ->
                        ( insert model val
                        , Cmd.none
                        )


subscribe : (Msg -> msg) -> Firebase.Database.Types.Database -> Config m v -> Sub msg
subscribe tagger db config =
    let
        ref : Firebase.Database.Types.Reference
        ref =
            Firebase.Database.ref (Just config.path) db
    in
        Sub.batch
            [ Time.every (Time.second * 4) (\time -> tagger (Time time))
            , Firebase.Database.Reference.on "child_added" ref (\snapshot -> tagger (Snapshot snapshot))
            , Firebase.Database.Reference.on "child_changed" ref (\snapshot -> tagger (Snapshot snapshot))
            , Firebase.Database.Reference.on "child_removed" ref (\snapshot -> tagger (Snapshot snapshot))
            , Firebase.Database.Reference.on "child_moved" ref (\snapshot -> tagger (Snapshot snapshot))
            ]
