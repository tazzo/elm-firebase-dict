module Config exposing (Config, empty, encodeConfig, decode)

import Json.Encode as JE
import Json.Decode as JD


type alias Config =
    { bool : Bool
    , string : String
    , int : Int
    }


empty : Config
empty =
    { bool = False
    , string = "empty"
    , int = 0
    }


encodeConfig : Config -> JE.Value
encodeConfig c =
    JE.object
        [ ( "bool", JE.bool c.bool )
        , ( "string", JE.string c.string )
        , ( "int", JE.int c.int )
        ]


configDecoder : JD.Decoder Config
configDecoder =
    JD.map3 Config
        (JD.at [ "bool" ] JD.bool)
        (JD.at [ "string" ] JD.string)
        (JD.at [ "int" ] JD.int)


decode : JD.Decoder Config -> JE.Value -> Maybe Config
decode d v =
    case (decodeConfig d v) of
        Ok c ->
            Just c

        Err s ->
            Nothing


decodeConfig : JD.Decoder Config -> JE.Value -> Result String Config
decodeConfig d v =
    JD.decodeValue d v
