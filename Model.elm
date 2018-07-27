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
      , messages =
            [ { message = "Buy groceries"
              , selected = False
              }
            , { message = "Feed cat"
              , selected = False
              }
            , { message = "Take out the bins"
              , selected = False
              }
            , { message = "Wash the dishes"
              , selected = False
              }
            ]
      , tempMessage = ""
      }
    , Cmd.none
    )
