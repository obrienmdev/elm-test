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
    { content : String
    , selected : String
    }


init : ( Model, Cmd Msg )
init =
    ( { content = "", selected = "none" }, Cmd.none )



-- UPDATE


type Msg
    = Increment
    | Edit String
    | Fetch
    | Receive (Result Http.Error String)
    | Select String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Edit newContent ->
            ( { model | content = newContent }, Cmd.none )

        Increment ->
            let
                str =
                    .content model
                        |> String.reverse
            in
                ( { model | content = str }, Cmd.none )

        Fetch ->
            ( model, fetch ("/" ++ model.content) )

        Receive (Ok data) ->
            ( { model | content = data }, Cmd.none )

        Receive (Err err) ->
            ( { model | content = toString err }, Cmd.none )

        Select data ->
            ( { model | selected = data }, Cmd.none )


fetch : String -> Cmd Msg
fetch url =
    let
        request =
            Http.getString url
    in
        Http.send Receive request



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "enter text", onInput Edit ] []
        , Html.br [] []
        , button [ onClick Increment ] [ text "button" ]
        , button [ onClick Fetch ] [ text "fetch!" ]
        , display model
        , options
        ]


col model =
    if String.length (.content model) > 5 then
        "red"
    else
        "green"


display : Model -> Html Msg
display model =
    let
        colour =
            col model
    in
        div []
            [ div [ style [ ( "color", colour ) ] ] [ text (.content model) ]
            , div [ style [ ( "color", "blue" ) ] ] [ text (.selected model) ]
            ]


options : Html Msg
options =
    fieldset [] (List.map option ["a","b"])

option : String -> Html Msg
option str =
    label []
        [ input [ type_ "radio", onClick (Select ("you picked " ++ str)) ] []
        , text str
        , Html.br [] []
        ]



-- SUBS


subscriptions model =
    Sub.none

