module FirebaseDict.FDict exposing (..)

import Dict
import FirebaseDict.Types exposing (..)


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


insert_ : String -> v -> FDict v -> FDict v
insert_ k v fd =
    { fd
        | values = Dict.insert k v fd.values
    }


remove_ : String -> FDict v -> FDict v
remove_ k fd =
    { fd
        | values = Dict.remove k fd.values
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
