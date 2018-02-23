module Main exposing (..)

import Html exposing (..)
import Material
import Material.Layout as Layout


--firebase

import FirebaseDict
import FirebaseDict.Types
import FirebaseDict.FDict as FDict


-- Data

import Data


-- Model and View

import Model exposing (..)
import View exposing (..)


dataConfig : FirebaseDict.Types.Config Model Data.Data
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
            let
                data : Maybe Data.Data
                data =
                    (FDict.get "tt" model.fooDict)
            in
                case data of
                    Nothing ->
                        ( model, Cmd.none )

                    Just d ->
                        let
                            newdata =
                                { d | string = "ciao a tutti" }
                        in
                            ( { model | fooDict = FDict.insert "tt" newdata model.fooDict }, Cmd.none )

        Set ->
            ( { model | fooDict = FDict.insert "tt" Data.empty model.fooDict }, Cmd.none )

        Get ->
            ( model, Cmd.none )

        -- Boilerplate: FirebaseDict action handler.
        FirebaseDictMsg msg ->
            FirebaseDict.update FirebaseDictMsg msg model dataConfig


view : Model -> Html Msg
view model =
    View.render model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Layout.subs Mdl model.mdl
        , FirebaseDict.subscribe FirebaseDictMsg model.db dataConfig
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( initModel, Layout.sub0 Mdl )
        , view = view
        , subscriptions = subscriptions
        , update = update
        }
