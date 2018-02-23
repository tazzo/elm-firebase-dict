module FirebaseDict exposing (..)

import FirebaseDict.FDict as FDict
import FirebaseDict.Types exposing (..)
import Json.Decode as JD
import Time exposing (Time)
import Task


-- Firebase

import Firebase.Database
import Firebase.Database.Snapshot
import Firebase.Database.Reference
import Firebase.Database.Types
import Firebase.Errors exposing (Error)


type Msg
    = Heartbeat Firebase.Database.Types.Reference
    | Snapshot Firebase.Database.Types.Snapshot
    | WriteStatus (Result Error ())
    | Remove Firebase.Database.Types.Snapshot



-- Do task 1, discard it's return value, then do task 2


(&>) t1 t2 =
    Task.andThen (\_ -> t2) t1


update : (Msg -> msg) -> Msg -> m -> Config m v -> ( m, Cmd msg )
update tagger msg model config =
    let
        _ =
            ""
    in
        case msg of
            Heartbeat ref ->
                let
                    command =
                        config.get model
                            |> FDict.toListWithEvents
                            |> List.map eventToTask
                            |> Cmd.batch

                    eventToTask ( k, event, maybe_value ) =
                        case event of
                            Set ->
                                case maybe_value of
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
                                    |> Task.attempt (\x -> tagger (WriteStatus x))
                in
                    ( config.set model (FDict.clearEvents (config.get model)), command )

            WriteStatus (Ok v) ->
                ( model
                , Cmd.none
                )

            WriteStatus (Err err) ->
                let
                    _ =
                        Debug.log "Firebase write fail : " err
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

            Remove snapshot ->
                let
                    ref =
                        Firebase.Database.Snapshot.ref snapshot

                    key =
                        Firebase.Database.Snapshot.key snapshot

                    delete m key =
                        case key of
                            Nothing ->
                                m

                            Just str ->
                                config.get m
                                    |> FDict.remove_ str
                                    |> config.set m

                    newModel =
                        delete model key
                in
                    ( newModel
                    , Cmd.none
                    )


subscribe : (Msg -> msg) -> Firebase.Database.Types.Database -> Config m v -> Sub msg
subscribe tagger db config =
    let
        ref =
            Firebase.Database.ref (Just config.path) db
    in
        Sub.batch
            [ Time.every (Time.second * 1) (\time -> tagger (Heartbeat ref))
            , Firebase.Database.Reference.on "child_added" ref (\snapshot -> tagger (Snapshot snapshot))
            , Firebase.Database.Reference.on "child_changed" ref (\snapshot -> tagger (Snapshot snapshot))
            , Firebase.Database.Reference.on "child_removed" ref (\snapshot -> tagger (Remove snapshot))

            -- , Firebase.Database.Reference.on "child_moved" ref (\snapshot -> tagger (Snapshot snapshot))
            ]
