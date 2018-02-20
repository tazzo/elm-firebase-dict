module FirebaseDict exposing (..)

import Json.Encode as JE
import Json.Decode as JD
import Dict
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
    , get : m -> Dict.Dict String v
    , set : m -> Dict.Dict String v -> m
    }


type alias FDict v =
    Dict.Dict String v


empty : FDict v
empty =
    Dict.empty


createFDict : Config m v -> FDict v
createFDict config =
    Dict.empty


create : String -> (v -> JE.Value) -> JD.Decoder v -> (m -> Dict.Dict String v) -> (m -> Dict.Dict String v -> m) -> Config m v
create path encoder decoder get set =
    { path = path
    , encoder = encoder
    , decoder = decoder
    , get = get
    , set = set
    }


type Msg
    = Time Time
    | Snapshot Firebase.Database.Types.Snapshot


update : (Msg -> msg) -> Msg -> Config m v -> m -> ( m, Cmd msg )
update tagger msg config model =
    let
        _ =
            Debug.log "update : " msg
    in
        case msg of
            Time time ->
                ( model, Cmd.none )

            Snapshot snapshot ->
                -- ( model, Cmd.none )
                let
                    value : Result String v
                    value =
                        snapshot
                            |> Firebase.Database.Snapshot.value
                            -- Gives us a Json.Decode.Value
                            |> JD.decodeValue config.decoder
                            -- Convert into a Result String a (where a is a String)
                            |> Debug.log "update Snapshot"

                    key =
                        Firebase.Database.Snapshot.key snapshot

                    insert m v =
                        case key of
                            Nothing ->
                                m

                            Just str ->
                                config.get m
                                    |> Dict.insert str v
                                    |> config.set m
                in
                    case value of
                        Err str ->
                            ( model, Cmd.none )

                        Ok val ->
                            ( insert model val
                            , Cmd.none
                            )


subscribeFDict : (Msg -> msg) -> Firebase.Database.Types.Database -> Config m v -> Sub msg
subscribeFDict tagger db config =
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
