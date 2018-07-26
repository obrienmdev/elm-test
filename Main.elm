module Main exposing (..)

import Html exposing (Html, button, div, fieldset, input, label, text)
import Html.Attributes exposing (placeholder, style, type_)
import Html.Events exposing (onClick, onInput)
import Http


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- MODEL

type alias Model =
    {
      message : String
    , messages : List String
    }


init : ( Model, Cmd Msg )
init =
    (
      { message = "", messages = [] }
      , Cmd.none
    )

-- UPDATE

type Msg
    = EditText String
    | Add

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditText message ->
          ( { model | message = message }, Cmd.none )

        Add ->
          ( { model | message = "", messages = (.message model) :: (.messages model) }, Cmd.none )

-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "enter text", onInput EditText ] []
        , Html.br [] []
        , button [ onClick Add ] [ text "add" ]
        , todos (.messages model)
        ]

todos : List String -> Html Msg
todos messages =
   fieldset [] (List.map todo messages)

todo : String -> Html Msg
todo message =
  div [] [text message, Html.br [] []]

-- SUBS

subscriptions model =
    Sub.none

