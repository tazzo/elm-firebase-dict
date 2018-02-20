module Data exposing (..)

import Json.Encode as JE
import Json.Decode as JD


createFDictConfig path get set =
    { path = path
    , encoder = encodeData
    , decoder = dataDecoder
    , get = get
    , set = set
    }


type alias Data =
    { bool : Bool
    , string : String
    , int : Int
    }


empty : Data
empty =
    { bool = False
    , string = "empty"
    , int = 0
    }


encodeData : Data -> JE.Value
encodeData c =
    JE.object
        [ ( "bool", JE.bool c.bool )
        , ( "string", JE.string c.string )
        , ( "int", JE.int c.int )
        ]


dataDecoder : JD.Decoder Data
dataDecoder =
    JD.map3 Data
        (JD.at [ "bool" ] JD.bool)
        (JD.at [ "string" ] JD.string)
        (JD.at [ "int" ] JD.int)


decode : JD.Decoder Data -> JE.Value -> Maybe Data
decode d v =
    case (decodeData d v) of
        Ok c ->
            Just c

        Err s ->
            Nothing


decodeData : JD.Decoder Data -> JE.Value -> Result String Data
decodeData d v =
    JD.decodeValue d v
