module FirebaseDict.Types exposing (..)

import Dict
import Json.Encode as JE
import Json.Decode as JD


type alias Config m v =
    { path : String
    , encoder : v -> JE.Value
    , decoder : JD.Decoder v
    , get : m -> FDict v
    , set : m -> FDict v -> m
    }


type Event
    = Set
    | Delete


type alias FDict v =
    { values : Dict.Dict String v
    , events : Dict.Dict String Event
    }
