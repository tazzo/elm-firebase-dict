module FirebaseDict exposing (..)

import FirebaseDict.FDict as FDict
import FirebaseDict.Types exposing (..)
import Json.Decode as JD
import Time exposing (Time)
import Task


-- Firebase

import Firebase.Database as Database
import Firebase.Database.Snapshot as Snapshot
import Firebase.Database.Reference as Reference
import Firebase.Database.Types as Types
import Firebase.Errors exposing (Error)


type Msg
    = Heartbeat Types.Reference
    | Snapshot Types.Snapshot
    | WriteStatus (Result Error ())
    | Remove Types.Snapshot



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
                        config.getDict model
                            |> FDict.toListWithEvents
                            |> List.map eventToTask
                            |> Cmd.batch

                    eventToTask ( k, event, maybe_value ) =
                        case event of
                            Set ->
                                case maybe_value of
                                    Just v ->
                                        ref
                                            |> Reference.child k
                                            |> Reference.set (config.encoder v)
                                            |> Task.attempt (\x -> tagger (WriteStatus x))

                                    Nothing ->
                                        Task.attempt (\x -> tagger (WriteStatus x)) (Task.succeed ())

                            Delete ->
                                ref
                                    |> Reference.child k
                                    |> Reference.remove
                                    |> Task.attempt (\x -> tagger (WriteStatus x))
                in
                    ( config.getDict model
                        |> FDict.clearEvents
                        |> config.setDict model
                    , command
                    )

            WriteStatus (Ok v) ->
                ( model
                , Cmd.none
                )

            WriteStatus (Err err) ->
                let
                    _ =
                        Debug.log "Firebase write fail : " err
                in
                    ( model, Cmd.none )

            Snapshot snapshot ->
                let
                    value : Result String v
                    value =
                        snapshot
                            |> Snapshot.value
                            |> JD.decodeValue config.decoder

                    key =
                        Snapshot.key snapshot

                    insert m v =
                        case key of
                            Nothing ->
                                m

                            Just str ->
                                config.getDict m
                                    |> FDict.insert_ str v
                                    |> config.setDict m
                in
                    case value of
                        Err str ->
                            ( model, Cmd.none )

                        Ok val ->
                            ( insert model val, Cmd.none )

            Remove snapshot ->
                let
                    key =
                        Snapshot.key snapshot

                    delete m key =
                        case key of
                            Nothing ->
                                m

                            Just str ->
                                config.getDict m
                                    |> FDict.remove_ str
                                    |> config.setDict m
                in
                    ( delete model key, Cmd.none )


newKey : Types.Database -> Config m v -> String
newKey db config =
    Database.ref (Just config.path) db
        |> Reference.push
        |> Reference.key


subscribe : (Msg -> msg) -> Types.Database -> Config m v -> Sub msg
subscribe tagger db config =
    let
        ref =
            Database.ref (Just config.path) db
    in
        Sub.batch
            [ Time.every (Time.second * 1) (\time -> tagger (Heartbeat ref))
            , Reference.on "child_added" ref (\snapshot -> tagger (Snapshot snapshot))
            , Reference.on "child_changed" ref (\snapshot -> tagger (Snapshot snapshot))
            , Reference.on "child_removed" ref (\snapshot -> tagger (Remove snapshot))

            -- , Firebase.Database.Reference.on "child_moved" ref (\snapshot -> tagger (Snapshot snapshot))
            ]
