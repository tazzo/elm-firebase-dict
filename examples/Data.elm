module Data exposing (..)

import Json.Encode as JE
import Json.Decode as JD


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


encoder : Data -> JE.Value
encoder c =
    JE.object
        [ ( "bool", JE.bool c.bool )
        , ( "string", JE.string c.string )
        , ( "int", JE.int c.int )
        ]


deoder : JD.Decoder Data
deoder =
    JD.map3 Data
        (JD.at [ "bool" ] JD.bool)
        (JD.at [ "string" ] JD.string)
        (JD.at [ "int" ] JD.int)
