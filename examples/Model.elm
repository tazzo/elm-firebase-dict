module Model exposing (..)

import Material
import Firebase
import Firebase.Database
import Firebase.Database.Types
import Firebase.Errors exposing (Error)
import Data
import FirebaseDict


type alias Model =
    { mdl : Material.Model
    , app : Firebase.App
    , db : Firebase.Database.Types.Database
    , onText : String
    , config : Data.Data
    , fooDict : FirebaseDict.FDict Data.Data
    }


type Msg
    = Mdl (Material.Msg Msg)
    | FooValue Firebase.Database.Types.Snapshot
    | Push
    | Set
    | Get
    | MyToggleMsg
    | ChangeStringMsg String
    | ChangeIntMsg String
    | WriteStatus (Result Error ())
    | HeartBit FirebaseDict.Msg


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
        , config = Data.empty
        , fooDict = FirebaseDict.empty
        }
