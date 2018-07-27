module Model exposing (Model, init)

import Msg exposing (Msg)
import Todo exposing (Todo)


type alias Model =
    { message : String -- main text box
    , messages : List Todo
    , tempMessage : String -- message being modified
    }


init : ( Model, Cmd Msg )
init =
    ( { message = ""
      , messages = []
      , tempMessage = ""
      }
    , Cmd.none
    )
