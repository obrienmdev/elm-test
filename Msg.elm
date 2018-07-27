module Msg exposing (..)


type Msg
    = EditText String
    | EditTempText String
    | Add
    | ModifyBegin Int
    | ModifyEnd Int String
    | ModifyCancel Int
    | Delete Int
