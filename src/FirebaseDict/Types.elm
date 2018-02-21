module FirebaseDict.Types exposing (..)

import Dict


type Event
    = Set
    | Delete


type alias FDict v =
    { values : Dict.Dict String v
    , events : Dict.Dict String Event
    }
