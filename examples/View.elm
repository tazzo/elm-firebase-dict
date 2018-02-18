module View exposing (..)

import Html exposing (..)
import Material.Layout as Layout
import Material.Toggles as Toggles
import Material.Color as Color
import Material.Card as Card
import Material.Textfield as Textfield
import Material.Options as Options
import Material.Options as Options exposing (css)
import Material.Elevation as Elevation
import Material.Button as Button
import Material.List as Lists
import Material.Grid exposing (stretch, grid, cell, size, order, offset, Device(..))
import MarkdownMath exposing (toHtml)
import Model exposing (..)


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
        [ text model.onText ]


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
                [ renderContents model ]
            ]


renderContents : Model -> Html Msg
renderContents model =
    Card.view
        [ css "width" "100%"
        , Elevation.e8

        -- ,Color.background (Color.color Color.Amber Color.S600)
        ]
        [ Card.text []
            [ toHtml [] (toString model.config)
            , renderConfig model
            ]
        ]


renderConfig : Model -> Html Msg
renderConfig model =
    Lists.ul []
        [ Lists.li []
            [ Lists.content []
                [ Toggles.switch Mdl
                    [ 0, 1, 3 ]
                    model.mdl
                    [ Options.onToggle MyToggleMsg
                    , Toggles.ripple
                    , Toggles.value model.config.bool
                    ]
                    [ text "bool" ]
                ]
            ]
        , Lists.li []
            [ Lists.content []
                [ Textfield.render Mdl
                    [ 0, 1, 4 ]
                    model.mdl
                    [ Textfield.label "string"
                    , Textfield.floatingLabel
                    , Textfield.value model.config.string
                    , Options.onInput ChangeStringMsg
                    ]
                    []
                ]
            ]
        , Lists.li []
            [ Lists.content []
                [ Textfield.render Mdl
                    [ 0, 1, 5 ]
                    model.mdl
                    [ Textfield.label "int"
                    , Textfield.floatingLabel
                    , Textfield.value <| toString model.config.int
                    , Options.onInput ChangeIntMsg
                    ]
                    []
                ]
            ]
        ]
