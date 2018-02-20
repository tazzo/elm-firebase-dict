module Model exposing (..)

import Material
import Firebase
import Firebase.Database
import Firebase.Database.Types
import Data
import FirebaseDict


type alias Model =
    { mdl : Material.Model
    , app : Firebase.App
    , db : Firebase.Database.Types.Database
    , fooDict : FirebaseDict.FDict Data.Data
    }


type Msg
    = Mdl (Material.Msg Msg)
    | Push
    | Set
    | Get
    | FirebaseDictMsg FirebaseDict.Msg


firebaseInit =
    { apiKey = "AIzaSyCYC8DiqgnpH5ea1FEwVAewNT-mBHB0-6U"
    , authDomain = "elm-firebase-try01.firebaseapp.com"
    , databaseURL = "https://elm-firebase-try01.firebaseio.com"
    , projectId = "elm-firebase-try01"
    , storageBucket = "elm-firebase-try01.appspot.com"
    , messagingSenderId = "747855250165"
    }


initModel : Model
initModel =
    let
        app =
            Firebase.init firebaseInit
    in
        { mdl = Material.model
        , app = app
        , db = Firebase.Database.init app
        , fooDict = FirebaseDict.empty
        }
