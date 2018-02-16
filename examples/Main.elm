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
    , text : String
    , app : Firebase.App
    , db : Firebase.Database.Types.Database
    , onText : String
    , tmp : Bool
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
        , text = example1
        , app = app
        , db = db
        , onText = "init"
        , tmp = True
        }


type Msg
    = Mdl (Material.Msg Msg)
    | InputChange String
    | FooValue Firebase.Database.Types.Snapshot


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Boilerplate: Mdl action handler.
        Mdl msg_ ->
            Material.update Mdl msg_ model

        InputChange str ->
            ( { model | text = str }, Cmd.none )

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
                ( { model | onText = str, tmp = False }
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
        (examplesList model)
    , Layout.navigation
        []
        [ Layout.title [] [ text "Github" ]
        , Layout.link
            [ Layout.href "https://github.com/tazzo/elm-markdown-math" ]
            [ text "elm-markdown-math" ]
        , Layout.link
            [ Layout.href "https://github.com/tazzo/elm-markdown-math-demo" ]
            [ text "demo" ]
        ]
    ]


examplesList model =
    [ Layout.title [] [ text "Examples" ]
    , button1 model
    ]


button1 model =
    Button.render Mdl
        [ 2, 1 ]
        model.mdl
        [ Button.ripple
        , Options.onClick <| InputChange example1
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
            [ size All 8
            , size Desktop 6
            , stretch
            ]
            [ renderMessage model ]
        , cell
            [ size All 8
            , size Desktop 6
            , stretch
            ]
            [ tf model ]
        ]


tf : Model -> Html Msg
tf model =
    Textfield.render Mdl
        [ 0, 9 ]
        model.mdl
        [ css "width" "100%"
        , css "padding-left" "10px"
        , css "padding-right" "10px"
        , Textfield.label "Enter Markdown and Math here"
        , Textfield.floatingLabel
        , Textfield.textarea
        , Textfield.rows 20
        , Textfield.value model.text
        , Options.onInput InputChange
        , Color.background Color.white
        , Elevation.e8
        ]
        []


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
            , toHtml [] model.text
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
        if model.tmp then
            let
                _ =
                    Debug.log "subscriptions " "True"
            in
                Sub.batch
                    [ Layout.subs Mdl model.mdl
                    , Firebase.Database.Reference.on "value" fooRef FooValue
                    ]
        else
            let
                _ =
                    Debug.log "subscriptions " "False"
            in
                Sub.batch
                    [ Layout.subs Mdl model.mdl
                    ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( initModel, Layout.sub0 Mdl )
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


example1 =
    """
### Markdown Math

Tex math **textstyle (default)** $$ \\int_{0}^{\\infty} e^{-x} dx$$

Tex math **textstyle** $$\\textstyle \\int_{0}^{\\infty} e^{-x} dx$$


#### math  with color
$$
 \\color{red}{
x^2-3 \\over x+1 }+123

 $$

#### limit
$$
\\displaystyle\\lim_{x \\to \\infty} e^{-x} = 0
 $$


"""
