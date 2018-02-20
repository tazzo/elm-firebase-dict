module Main exposing (..)

import Html exposing (..)
import Material
import Material.Layout as Layout


--firebase

import FirebaseDict


-- Data

import Data


-- Model and View

import Model exposing (..)
import View exposing (..)


dataConfig : FirebaseDict.Config Model Data.Data
dataConfig =
    { path = "foo"
    , encoder = Data.encoder
    , decoder = Data.deoder
    , get = .fooDict
    , set = \m v -> { m | fooDict = v }
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Boilerplate: Mdl action handler.
        Mdl msg_ ->
            Material.update Mdl msg_ model

        Push ->
            ( model, Cmd.none )

        Set ->
            ( model, Cmd.none )

        Get ->
            ( model, Cmd.none )

        FirebaseDictMsg msg ->
            FirebaseDict.update FirebaseDictMsg msg dataConfig model


view : Model -> Html Msg
view model =
    View.render model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Layout.subs Mdl model.mdl
        , FirebaseDict.subscribeFDict FirebaseDictMsg model.db dataConfig
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( initModel, Layout.sub0 Mdl )
        , view = view
        , subscriptions = subscriptions
        , update = update
        }
