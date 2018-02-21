module Data exposing (..)

import Json.Encode as JE
import Json.Decode as JD
import Material.Typography as Typo
import Material.Options as Options
import Html exposing (..)
import Html exposing (..)
import Material.Card as Card
import Material.Options as Options
import Material.Options as Options exposing (css)
import Material.Elevation as Elevation
import Material.Button as Button


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


render : m -> Data -> Html msg
render data model =
    Card.view
        [ css "width" "100%"
        , Elevation.e2
        , css "margin" "4px 8px 10px 0px"
        ]
        [ Card.text []
            [ Options.styled p
                [ Typo.body1 ]
                [ text (toString data) ]
            ]

        -- , Card.actions
        --     [ Card.border, css "vertical-align" "center", css "text-align" "right", white ]
        --     [ Button.render Mdl
        --         [ 8, 1 ]
        --         model.mdl
        --         [ Button.icon, Button.ripple ]
        --         [ Icon.i "favorite_border" ]
        --     , Button.render Mdl
        --         [ 8, 2 ]
        --         model.mdl
        --         [ Button.icon, Button.ripple ]
        --         [ Icon.i "event_available" ]
        --     ]
        ]


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
