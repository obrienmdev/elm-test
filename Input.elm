module Input exposing (textInput, addTodo)

import Html exposing (Html, button, input, text)
import Html.Attributes exposing (placeholder, style, value)
import Html.Events exposing (onClick, onInput)
import Model exposing (Model)
import Msg exposing (..)


textInput : Model -> Html Msg
textInput model =
    input
        [ placeholder "enter text"
        , onInput EditText
        , style textInputStyle
        , value model.message
        ]
        []


textInputStyle : List ( String, String )
textInputStyle =
    List.append inputStyle [ ( "width", "500px" ) ]


addTodo : Html Msg
addTodo =
    button
        [ onClick Add
        , style (inputStyle)
        ]
        [ text "add" ]


inputStyle : List ( String, String )
inputStyle =
    [ ( "margin", "10px" )
    , ( "width", "100px" )
    , ( "height", "40px" )
    ]
