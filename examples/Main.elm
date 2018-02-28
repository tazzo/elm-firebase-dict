module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Json.Encode as JE
import Json.Decode as JD


-- MDL

import Material
import Material.Icon as Icon
import Material.Layout as Layout
import Material.Color as Color
import Material.Card as Card
import Material.Textfield as Textfield
import Material.Options as Options
import Material.Options as Options exposing (css)
import Material.Elevation as Elevation
import Material.Button as Button
import Material.Grid exposing (stretch, grid, cell, size, order, offset, Device(..))
import Material.Typography as Typo


--firebase

import FDict


-- Model -------------------------


type alias Model =
    { mdl : Material.Model
    , fooDict : FDict.FDict Todo
    , text : String
    }


type Msg
    = Mdl (Material.Msg Msg)
    | FDictMsg FDict.Msg
    | Set
    | UpdateField String
    | Add
    | ToggleMsg String
    | DeleteMsg String


initModel : Model
initModel =
    { mdl = Material.model
    , fooDict = FDict.empty
    , text = ""
    }



-- Todo data--------------------


firebaseConfig =
    { apiKey = "AIzaSyCYC8DiqgnpH5ea1FEwVAewNT-mBHB0-6U"
    , authDomain = "elm-firebase-try01.firebaseapp.com"
    , databaseURL = "https://elm-firebase-try01.firebaseio.com"
    , projectId = "elm-firebase-try01"
    , storageBucket = "elm-firebase-try01.appspot.com"
    , messagingSenderId = "747855250165"
    }


encoder =
    \c ->
        JE.object
            [ ( "bool", JE.bool c.bool )
            , ( "string", JE.string c.string )
            ]


decoder =
    JD.map2 Todo
        (JD.at [ "bool" ] JD.bool)
        (JD.at [ "string" ] JD.string)


todoManager : FDict.Manager Model Todo
todoManager =
    FDict.initManager
        firebaseConfig
        "foo"
        encoder
        decoder
        (.fooDict)
        (\m v -> { m | fooDict = v })


type alias Todo =
    { bool : Bool
    , string : String
    }



-- Update ------------------------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Boilerplate: Mdl action handler.
        Mdl msg_ ->
            Material.update Mdl msg_ model

        -- Boilerplate: FirebaseDict action handler.
        FDictMsg msg ->
            FDict.update FDictMsg msg model todoManager

        Set ->
            ( { model | fooDict = FDict.remove "tt" model.fooDict }
            , Cmd.none
            )

        Add ->
            let
                todo =
                    Todo False model.text
            in
                ( FDict.insertInModel
                    model
                    todoManager
                    (FDict.newKey todoManager)
                    todo
                , Cmd.none
                )

        UpdateField str ->
            ( { model | text = str }, Cmd.none )

        ToggleMsg key ->
            let
                newModel maybetodo =
                    case maybetodo of
                        Nothing ->
                            model

                        Just todo ->
                            let
                                newDict =
                                    FDict.insert key { todo | bool = not todo.bool } model.fooDict
                            in
                                { model | fooDict = newDict }
            in
                ( model.fooDict
                    |> FDict.get key
                    |> newModel
                , Cmd.none
                )

        DeleteMsg key ->
            ( { model
                | fooDict =
                    model.fooDict
                        |> FDict.remove key
              }
            , Cmd.none
            )



-- Subscriptions -----------------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Layout.subs Mdl model.mdl
        , FDict.subscribe FDictMsg todoManager
        ]



-- Main  -----------------------------------


main : Program Never Model Msg
main =
    Html.program
        { init = ( initModel, Layout.sub0 Mdl )
        , view = view
        , subscriptions = subscriptions
        , update = update
        }



-- View -----------------------------------


view : Model -> Html Msg
view model =
    renderView model


renderView : Model -> Html Msg
renderView model =
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
        [ Layout.title [] [ text "Github" ]
        , Layout.link
            [ Layout.href "https://github.com/tazzo/elm-firebase-dict" ]
            [ text "elm-firebase-dict" ]
        ]
    ]


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

        -- onEnter ------------------
        onEnter msg =
            let
                isEnter code =
                    if code == 13 then
                        JD.succeed msg
                    else
                        JD.fail "not ENTER"
            in
                Options.on "keydown" (JD.andThen isEnter keyCode)
    in
        grid [ Color.background (Color.color Color.Grey Color.S100) ]
            [ cell
                [ size All 12
                , stretch
                ]
                [ Textfield.render Mdl
                    [ 2 ]
                    model.mdl
                    [ Textfield.label "TODO"
                    , Textfield.floatingLabel
                    , Textfield.text_
                    , Textfield.value model.text
                    , Options.onInput UpdateField
                    , onEnter Add
                    ]
                    []
                ]
            , cell
                [ size All 6
                , stretch
                ]
                (renderContents model)
            ]


renderContents : Model -> List (Html Msg)
renderContents model =
    model.fooDict
        |> FDict.toList
        |> List.reverse
        |> List.indexedMap (renderData model)


renderData : Model -> Int -> ( String, Todo ) -> Html Msg
renderData model i ( key, todo ) =
    let
        ( imageString, colorTodo, actionColor ) =
            case todo.bool of
                True ->
                    ( "check_box"
                    , Color.color Color.Grey Color.S600
                    , Color.color Color.Grey Color.S800
                    )

                False ->
                    ( "check_box_outline_blank"
                    , Color.color Color.Grey Color.S200
                    , Color.color Color.Grey Color.S600
                    )

        deleteAction =
            case todo.bool of
                True ->
                    [ Layout.spacer
                    , Button.render Mdl
                        [ 8, i + 100 ]
                        model.mdl
                        [ Button.icon, Button.ripple, Options.onClick <| DeleteMsg key ]
                        [ Icon.i "highlight_off" ]
                    ]

                False ->
                    []
    in
        Card.view
            [ Color.background colorTodo
            , css "width" "100%"
            , Elevation.e2
            , css "margin" "4px 8px 10px 0px"
            ]
            [ Card.text
                []
                [ Options.styled p
                    [ Typo.body1 ]
                    [ text todo.string
                    ]
                ]
            , Card.actions
                [ Card.border
                , css "display" "flex"
                , Color.text actionColor
                ]
                (Button.render Mdl
                    [ 8, i ]
                    model.mdl
                    [ Button.icon, Button.ripple, Options.onClick <| ToggleMsg key ]
                    [ Icon.i imageString ]
                    :: deleteAction
                )
            ]
