module Model exposing (Model, init)

import Msg exposing (Msg)


type alias Model =
    { message : String
    , messages : List String
    }


init : ( Model, Cmd Msg )
init =
    ( { message = "", messages = [] }
    , Cmd.none
    )
