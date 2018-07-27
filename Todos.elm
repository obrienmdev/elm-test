module Todos exposing (todos, todo)

import Html exposing (Html, fieldset, div, button, text, br)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Msg exposing (..)


todos : List String -> Html Msg
todos messages =
    fieldset
        [ style [ ( "margin", "20px" ) ]
        ]
        (List.indexedMap todo messages)


todo : Int -> String -> Html Msg
todo i message =
    div []
        [ text message
        , button
            [ onClick
                (Delete i)
            , style
                [ ( "float", "right" )
                , ( "display", "inline-block" )
                ]
            ]
            [ text "delete" ]
        , Html.br
            [ style [ ( "clear", "right" ) ]
            ]
            []
        ]
