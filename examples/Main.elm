module Main exposing (..)

import Html exposing (..)
import Material
import Material.Layout as Layout
import Json.Decode as JD
import Json.Encode as JE
import Task


--firebase

import Firebase.Database
import Firebase.Database.Types
import Firebase.Database.Reference
import Firebase.Database.Snapshot


-- Config

import Config


-- Model and View

import Model exposing (..)
import View exposing (..)


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
                value : Result String Config.Config
                value =
                    snapshot
                        |> Firebase.Database.Snapshot.value
                        -- Gives us a Json.Decode.Value
                        |> JD.decodeValue Config.configDecoder
                        -- Convert into a Result String a (where a is a String)
                        |> Debug.log "FooValue.value.result"

                c =
                    case value of
                        Ok c ->
                            c

                        Err str ->
                            model.config

                -- Output the result (either `Err decodeMessage` or `Ok value`)
            in
                ( { model | config = c }
                , Cmd.none
                )

        Push ->
            ( model, Cmd.none )

        Get ->
            ( model, Cmd.none )

        MyToggleMsg ->
            let
                toggle c =
                    { c | bool = not c.bool }
            in
                ( { model | config = (toggle model.config) }, Cmd.none )

        ChangeStringMsg str ->
            let
                change c =
                    { c | string = str }
            in
                ( { model | config = (change model.config) }, Cmd.none )

        ChangeIntMsg str ->
            let
                change c =
                    case String.toInt str of
                        Ok i ->
                            { c | int = i }

                        Err s ->
                            c
            in
                ( { model | config = (change model.config) }, Cmd.none )

        Set ->
            let
                value : JE.Value
                value =
                    Config.encodeConfig model.config

                fooRef : Firebase.Database.Types.Reference
                fooRef =
                    model.db
                        |> Firebase.Database.ref (Just "foo")

                command : Cmd Msg
                command =
                    fooRef
                        |> Firebase.Database.Reference.set value
                        |> Task.attempt WriteStatus
            in
                ( model
                , command
                )

        WriteStatus (Ok _) ->
            let
                _ =
                    Debug.log "Firebase write success"
            in
                ( model
                , Cmd.none
                )

        WriteStatus (Err _) ->
            let
                _ =
                    Debug.log "Firebase write fail"
            in
                ( model
                , Cmd.none
                )


view : Model -> Html Msg
view model =
    Layout.render Mdl
        model.mdl
        [ Layout.fixedHeader
        , Layout.fixedDrawer
        ]
        { header = View.header model
        , drawer = drawer model
        , tabs = ( [], [] )
        , main = [ viewBody model ]
        }


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

            --  , Firebase.Database.Reference.on "value" fooRef FooValue
            , Firebase.Database.Reference.on "child_added" fooRef FooValue
            , Firebase.Database.Reference.on "child_changed" fooRef FooValue
            , Firebase.Database.Reference.on "child_removed" fooRef FooValue
            ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( initModel, Layout.sub0 Mdl )
        , view = view
        , subscriptions = subscriptions
        , update = update
        }
