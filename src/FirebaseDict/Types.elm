module FirebaseDict.Types exposing (..)

import Dict
import Json.Encode as JE
import Json.Decode as JD


type alias Config m v =
    { path : String
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
