module Main exposing (..)

import Html exposing (Html, button, div, fieldset, input, label, text)
import Html.Attributes exposing (placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Add exposing (add)
import Delete exposing (delete)
import Model exposing (Model, init)
import Msg exposing (..)
import Todos exposing (todos, todo)
import Input exposing (textInput, addTodo)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditText message ->
            ( { model | message = message }, Cmd.none )

        Add ->
            ( add model, Cmd.none )

        Delete index ->
            ( delete index model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ textInput model
        , addTodo
        , todos (.messages model)
        ]



-- SUBS


subscriptions model =
    Sub.none
