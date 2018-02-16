module Main exposing (..)

import Html exposing (..)
import Dict exposing (Dict)
import Material
import Material.Layout as Layout
import Material.Color as Color
import Material.Card as Card
import Material.Textfield as Textfield
import Material.Options as Options
import Material.Options as Options exposing (css)
import Material.Elevation as Elevation
import Material.Button as Button
import Material.Grid exposing (stretch, grid, cell, size, order, offset, Device(..))
import MarkdownMath exposing (toHtml)
import Json.Decode


--firebase

import Firebase
import Firebase.Database
import Firebase.Database.Types
import Firebase.Database.Reference
import Firebase.Database.Snapshot


type alias Model =
    { mdl : Mdl
    , app : Firebase.App
    , db : Firebase.Database.Types.Database
    , onText : String
    }


initModel : Model
initModel =
    let
        app : Firebase.App
        app =
            Firebase.init
                { apiKey = "AIzaSyCYC8DiqgnpH5ea1FEwVAewNT-mBHB0-6U"
                , authDomain = "elm-firebase-try01.firebaseapp.com"
                , databaseURL = "https://elm-firebase-try01.firebaseio.com"
                , projectId = "elm-firebase-try01"
                , storageBucket = "elm-firebase-try01.appspot.com"
                , messagingSenderId = "747855250165"
                }

        {-
           It's not necessary to store the database, but it will make it easier
           since all your database interactions are going to either be in `update`
           or `subscriptions`, and both have access to your model.
        -}
        db : Firebase.Database.Types.Database
        db =
            Firebase.Database.init app
    in
        { mdl =
            Material.model
        , app = app
        , db = db
        , onText = "init"
        }


type Msg
    = Mdl (Material.Msg Msg)
    | FooValue Firebase.Database.Types.Snapshot


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Boilerplate: Mdl action handler.
        Mdl msg_ ->
            Material.update Mdl msg_ model

        FooValue snapshot ->
            let
                {-
                   This decodes the value of "/foo" as a string.
                -}
                value : Result String String
                value =
                    snapshot
                        |> Firebase.Database.Snapshot.value
                        -- Gives us a Json.Decode.Value
                        |> Json.Decode.decodeValue Json.Decode.string
                        -- Convert into a Result String a (where a is a String)
                        |> Debug.log "FooValue.value.result"

                str =
                    case value of
                        Ok str ->
                            str

                        Err str ->
                            str

                -- Output the result (either `Err decodeMessage` or `Ok value`)
            in
                ( { model | onText = str }
                , Cmd.none
                )


type alias Mdl =
    Material.Model


view : Model -> Html Msg
view model =
    Layout.render Mdl
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
    grid [ Color.background (Color.color Color.Grey Color.S100) ]
        [ cell
            [ size All 12
            , size Desktop 12
            , stretch
            ]
            [ renderMessage model ]
        ]


renderMessage : Model -> Html Msg
renderMessage model =
    Card.view
        [ css "width" "100%"
        , Elevation.e8

        -- ,Color.background (Color.color Color.Amber Color.S600)
        ]
        [ Card.text []
            [ Button.render Mdl
                [ 0, 11 ]
                model.mdl
                [ Button.raised

                --, Options.onClick MyClickMsg
                ]
                [ text "Raised button" ]
            , toHtml [] "<h1>ciao</h1>"
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        fooRef : Firebase.Database.Types.Reference
        fooRef =
            model.db
                |> Firebase.Database.ref (Just "foo")
    in
        Sub.batch
            [ Layout.subs Mdl model.mdl
            , Firebase.Database.Reference.on "value" fooRef FooValue
            ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( initModel, Layout.sub0 Mdl )
        , view = view
        , subscriptions = subscriptions
        , update = update
        }
