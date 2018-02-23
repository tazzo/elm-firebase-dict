module View exposing (..)

import Html exposing (..)
import Material.Layout as Layout
import Material.Color as Color
import Material.Card as Card
import Material.Options as Options
import Material.Options as Options exposing (css)
import Material.Elevation as Elevation
import Material.Button as Button
import Material.Icon as Icon
import Material.Grid exposing (stretch, grid, cell, size, order, offset, Device(..))
import Model exposing (..)
import FirebaseDict.FDict as FDict
import Data
import Material.Typography as Typo


render : Model -> Html Msg
render model =
    Layout.render
        Mdl
        model.mdl
        [ Layout.fixedHeader
        , Layout.fixedDrawer
        ]
        { header = header model
        , drawer = drawer model
        , tabs = ( [], [] )
        , main = [ viewBody model ]
        }


drawer : Model -> List (Html Msg)
drawer model =
    [ Layout.navigation
        []
        [ Layout.title [] [ text "Examples" ]
        , button1 model
        ]
    , Layout.navigation
        []
        [ Layout.title [] [ text "Github" ]
        , Layout.link
            [ Layout.href "https://github.com/tazzo/elm-firebase-dict" ]
            [ text "elm-firebase-dict" ]
        ]
    ]


button1 : Model -> Html Msg
button1 model =
    Button.render Mdl
        [ 2, 1 ]
        model.mdl
        [ Button.ripple

        -- , Options.onClick <| InputChange example1
        ]
        [ text "button1" ]


header : Model -> List (Html Msg)
header model =
    [ Layout.row
        [ css "transition" "height 333ms ease-in-out 0s"
        ]
        [ Layout.title [] [ text "Elm Firebase Dict Sync - Demo" ]
        ]
    ]


viewBody : Model -> Html Msg
viewBody model =
    let
        createButton model mdlMsg mdlId txt tagger =
            Button.render Mdl
                [ 0, 13 ]
                model.mdl
                [ Button.raised
                , Options.onClick tagger
                ]
                [ text txt ]
    in
        grid [ Color.background (Color.color Color.Grey Color.S100) ]
            [ cell
                [ size All 1
                , stretch
                ]
                [ createButton model Mdl [ 0, 11 ] "Push" Push ]
            , cell
                [ size All 1
                , stretch
                ]
                [ createButton model Mdl [ 0, 12 ] "Once" Get ]
            , cell
                [ size All 1
                , stretch
                ]
                [ createButton model Mdl [ 0, 13 ] "Set" Set ]
            , cell
                [ size All 12
                , stretch
                ]
                (renderContents model)
            ]


renderContents : Model -> List (Html Msg)
renderContents model =
    model.fooDict
        |> FDict.values
        |> List.indexedMap (renderData model)


renderData : Model -> Int -> Data.Data -> Html Msg
renderData model i data =
    Card.view
        [ Color.background (Color.color Color.Grey Color.S200)
        , css "width" "100%"
        , Elevation.e2
        , css "margin" "4px 8px 10px 0px"
        ]
        [ Card.text []
            [ Options.styled p
                [ Typo.body1 ]
                [ text (toString data) ]
            ]

        -- , Card.actions
        --     [ Card.border
        --     , css "vertical-align" "center"
        --     , css "text-align" "right"
        --     , Color.text Color.accent
        --     ]
        --     [ Button.render Mdl
        --         [ 8, i ]
        --         model.mdl
        --         [ Button.icon, Button.ripple ]
        --         [ Icon.i "create" ]
        --     , Button.render Mdl
        --         [ 8, i + 10 ]
        --         model.mdl
        --         [ Button.icon, Button.ripple ]
        --         [ Icon.i "check" ]
        --     ]
        ]
