module FirebaseDict exposing (..)

import FirebaseDict.FDict as FDict
import FirebaseDict.Types exposing (..)
import Json.Encode as JE
import Json.Decode as JD
import Time exposing (Time)
import Task


-- Firebase

import Firebase.Database
import Firebase.Database.Snapshot
import Firebase.Database.Reference
import Firebase.Database.Types
import Firebase.Errors exposing (Error)


type alias Config m v =
    { path : String
    , encoder : v -> JE.Value
    , decoder : JD.Decoder v
    , get : m -> FDict v
    , set : m -> FDict v -> m
    }


type Msg
    = Heartbeat Firebase.Database.Types.Reference
    | Snapshot Firebase.Database.Types.Snapshot
    | WriteStatus (Result Error ())



-- Do task 1, discard it's return value, then do task 2


(&>) t1 t2 =
    Task.andThen (\_ -> t2) t1


update : (Msg -> msg) -> Msg -> m -> Config m v -> ( m, Cmd msg )
update tagger msg model config =
    let
        _ =
            Debug.log "update entry "
    in
        case msg of
            Heartbeat ref ->
                let
                    _ =
                        Debug.log "Heartbeat " ""

                    command =
                        config.get model
                            |> FDict.toListWithEvents
                            |> List.map eventToTask
                            |> Cmd.batch

                    eventToTask ( k, e, mbv ) =
                        case e of
                            Set ->
                                case mbv of
                                    Just v ->
                                        ref
                                            |> Firebase.Database.Reference.child k
                                            |> Firebase.Database.Reference.set (config.encoder v)
                                            |> Task.attempt (\x -> tagger (WriteStatus x))

                                    Nothing ->
                                        Task.attempt (\x -> tagger (WriteStatus x)) (Task.succeed ())

                            Delete ->
                                ref
                                    |> Firebase.Database.Reference.child k
                                    |> Firebase.Database.Reference.remove
                                    |> Task.attempt (\v -> tagger (WriteStatus v))
                in
                    ( config.set model (FDict.clearEvents (config.get model)), command )

            WriteStatus (Ok _) ->
                let
                    _ =
                        Debug.log "Firebase write success"
                in
                    ( model
                    , Cmd.none
                    )

            WriteStatus (Err _) ->
                let
                    _ =
                        Debug.log "Firebase write fail"
                in
                    ( model
                    , Cmd.none
                    )

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
                                    |> FDict.insert_ str v
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
        ref =
            Firebase.Database.ref (Just config.path) db
    in
        Sub.batch
            [ Time.every (Time.second * 2) (\time -> tagger (Heartbeat ref))
            , Firebase.Database.Reference.on "child_added" ref (\snapshot -> tagger (Snapshot snapshot))
            , Firebase.Database.Reference.on "child_changed" ref (\snapshot -> tagger (Snapshot snapshot))
            , Firebase.Database.Reference.on "child_removed" ref (\snapshot -> tagger (Snapshot snapshot))
            , Firebase.Database.Reference.on "child_moved" ref (\snapshot -> tagger (Snapshot snapshot))
            ]
