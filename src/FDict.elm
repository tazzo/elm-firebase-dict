module FDict exposing (..)

import Json.Decode as JD
import Json.Encode as JE
import Time exposing (Time)
import Task
import Dict


-- Firebase

import Firebase
import Firebase.Database as Database
import Firebase.Database.Snapshot as Snapshot
import Firebase.Database.Reference as Reference
import Firebase.Database.Types as Types
import Firebase.Errors exposing (Error)


type alias Manager m v =
    { path : String
    , db : Types.Database
    , encoder : v -> JE.Value
    , decoder : JD.Decoder v
    , getDict : m -> FDict v
    , setDict : m -> FDict v -> m
    }


type Event
    = Set
    | Delete


type alias FDict v =
    { values : Dict.Dict String v
    , events : Dict.Dict String Event
    }


type Msg
    = Heartbeat Types.Reference
    | Snapshot Types.Snapshot
    | WriteStatus (Result Error ())
    | Remove Types.Snapshot


initManager :
    Firebase.Config
    -> String
    -> (v -> JE.Value)
    -> JD.Decoder v
    -> (m -> FDict v)
    -> (m -> FDict v -> m)
    -> Manager m v
initManager configFirebase path encoder decoder get set =
    let
        app =
            Firebase.init configFirebase
    in
        { path = path
        , db = Database.init app
        , encoder = encoder
        , decoder = decoder
        , getDict = get
        , setDict = set
        }



-- Do task 1, discard it's return value, then do task 2


(&>) t1 t2 =
    Task.andThen (\_ -> t2) t1


update : (Msg -> msg) -> Msg -> m -> Manager m v -> ( m, Cmd msg )
update tagger msg model manager =
    case msg of
        Heartbeat ref ->
            let
                command =
                    manager.getDict model
                        |> toListWithEvents
                        |> List.map eventToTask
                        |> Cmd.batch

                eventToTask ( k, event, maybe_value ) =
                    case event of
                        Set ->
                            case maybe_value of
                                Just v ->
                                    ref
                                        |> Reference.child k
                                        |> Reference.set (manager.encoder v)
                                        |> Task.attempt (\x -> tagger (WriteStatus x))

                                Nothing ->
                                    Task.attempt (\x -> tagger (WriteStatus x)) (Task.succeed ())

                        Delete ->
                            ref
                                |> Reference.child k
                                |> Reference.remove
                                |> Task.attempt (\x -> tagger (WriteStatus x))
            in
                ( manager.getDict model
                    |> clearEvents
                    |> manager.setDict model
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
                        |> JD.decodeValue manager.decoder

                key =
                    Snapshot.key snapshot

                insert m v =
                    case key of
                        Nothing ->
                            m

                        Just str ->
                            manager.getDict m
                                |> insert_ str v
                                |> manager.setDict m
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
                            manager.getDict m
                                |> remove_ str
                                |> manager.setDict m
            in
                ( delete model key, Cmd.none )


newKey : Manager m v -> String
newKey manager =
    Database.ref (Just manager.path) manager.db
        |> Reference.push
        |> Reference.key


subscribe : (Msg -> msg) -> Manager m v -> Sub msg
subscribe tagger manager =
    let
        ref =
            Database.ref (Just manager.path) manager.db
    in
        Sub.batch
            [ Time.every (Time.second * 1) (\time -> tagger (Heartbeat ref))
            , Reference.on "child_added" ref (\snapshot -> tagger (Snapshot snapshot))
            , Reference.on "child_changed" ref (\snapshot -> tagger (Snapshot snapshot))
            , Reference.on "child_removed" ref (\snapshot -> tagger (Remove snapshot))

            -- , Firebase.Database.Reference.on "child_moved" ref (\snapshot -> tagger (Snapshot snapshot))
            ]



-- build --------------


empty : FDict v
empty =
    { values = Dict.empty
    , events = Dict.empty
    }


insert : String -> v -> FDict v -> FDict v
insert k v fd =
    { fd
        | values = Dict.insert k v fd.values
        , events = Dict.insert k Set fd.events
    }


insertInModel : m -> Manager m v -> String -> v -> m
insertInModel model manager k v =
    manager.getDict model
        |> insert k v
        |> manager.setDict model


remove : String -> FDict v -> FDict v
remove k fd =
    { fd
        | values = Dict.remove k fd.values
        , events = Dict.insert k Delete fd.events
    }


insert_ : String -> v -> FDict v -> FDict v
insert_ k v fd =
    { fd
        | values = Dict.insert k v fd.values
    }


remove_ : String -> FDict v -> FDict v
remove_ k fd =
    { fd
        | values = Dict.remove k fd.values
        , events = Dict.remove k fd.events
    }


clearEvents : FDict v -> FDict v
clearEvents fd =
    { fd
        | events = Dict.empty
    }



-- query --------------


isEmpty : FDict v -> Bool
isEmpty fd =
    Dict.isEmpty fd.values


member : String -> FDict v -> Bool
member k fd =
    Dict.member k fd.values


get : String -> FDict v -> Maybe v
get k fd =
    Dict.get k fd.values


size : FDict v -> Int
size fd =
    Dict.size fd.values



-- lists --------------


keys : FDict v -> List String
keys fd =
    Dict.keys fd.values


values : FDict v -> List v
values fd =
    Dict.values fd.values


toListEvents : FDict v -> List ( String, Event )
toListEvents fd =
    Dict.toList fd.events


toListWithEvents : FDict v -> List ( String, Event, Maybe v )
toListWithEvents fd =
    Dict.toList fd.events
        |> List.map (\( k, e ) -> ( k, e, get k fd ))


toList : FDict v -> List ( String, v )
toList fd =
    Dict.toList fd.values


fromList : List ( String, v ) -> FDict v
fromList l =
    { values = Dict.fromList l
    , events = Dict.empty
    }
