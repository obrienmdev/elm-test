module Todos exposing (todos, todo)

import Html exposing (Html, fieldset, div, button, input, text, br)
import Html.Attributes exposing (style, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Model exposing (Model)
import Msg exposing (..)
import Todo exposing (Todo)


todos : Model -> Html Msg
todos model =
    let
        m =
            .messages model
    in
        fieldset
            [ style [ ( "margin", "20px" ) ]
            ]
            (List.indexedMap (todo model) m)


todo : Model -> Int -> Todo -> Html Msg
todo model i todo =
    div []
        [ todoText model todo i
        , todoButtons model todo i
        , Html.br
            [ style [ ( "clear", "right" ) ]
            ]
            []
        ]


todoText : Model -> Todo -> Int -> Html Msg
todoText model todo i =
    if (.selected todo) then
        input
            [ onInput EditTempText
            , value (.tempMessage model)
            ]
            []
    else
        text (.message todo)


todoButtons : Model -> Todo -> Int -> Html Msg
todoButtons model todo i =
    if (.selected todo) then
        div [ style (todoStyle todo) ]
            [ button
                [ onClick (ModifyEnd i (.tempMessage model))
                ]
                [ text "update" ]
            , button
                [ onClick (ModifyCancel i)
                ]
                [ text "cancel" ]
            ]
    else
        div [ style (todoStyle todo) ]
            [ button
                [ onClick (ModifyBegin i)
                ]
                [ text "edit" ]
            , button
                [ onClick
                    (Delete i)
                ]
                [ text "delete" ]
            ]


todoStyle : Todo -> List ( String, String )
todoStyle todo =
    [ ( "float", "right" )
    , ( "display", "inline-block" )
    ]
