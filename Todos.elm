module Todos exposing (todos, todo)

import Html exposing (Html, fieldset, div, button, input, text, br)
import Html.Attributes exposing (style, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Model exposing (Model)
import Msg exposing (..)
import Todo exposing (Todo)
import Style


todos : Model -> Html Msg
todos model =
    let
        m =
            .messages model
    in
        div
            [ style [ ( "margin", "20px" ), ( "font-size", "36px" ) ]
            ]
            (List.indexedMap (todo model) m)


todo : Model -> Int -> Todo -> Html Msg
todo model i todo =
    let
        s =
            if (.selected todo) then
                [ ( "background", "#eeeeff" ) ]
            else
                []
    in
        div [ style s ]
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
            , style Style.globalStyle
            ]
            []
    else
        text (.message todo)


todoButtons : Model -> Todo -> Int -> Html Msg
todoButtons model todo i =
    let
        buttonStyle =
            [ ( "float", "right" )
            , ( "display", "inline-block" )
            ]
    in
        if (.selected todo) then
            div [ style buttonStyle ]
                [ button
                    [ onClick (ModifyEnd i (.tempMessage model))
                    , style Style.globalStyle
                    ]
                    [ text "update" ]
                , button
                    [ onClick (ModifyCancel i)
                    , style Style.globalStyle
                    ]
                    [ text "cancel" ]
                ]
        else
            div [ style buttonStyle ]
                [ button
                    [ onClick (ModifyBegin i)
                    , style Style.globalStyle
                    ]
                    [ text "edit" ]
                , button
                    [ onClick
                        (Delete i)
                    , style Style.globalStyle
                    ]
                    [ text "delete" ]
                ]
