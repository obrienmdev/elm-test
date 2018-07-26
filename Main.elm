module Main exposing (..)

import Html exposing (Html, button, div, fieldset, input, label, text)
import Html.Attributes exposing (placeholder, style, type_, value)
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
    { message : String
    , messages : List String
    }


init : ( Model, Cmd Msg )
init =
    ( { message = "", messages = [] }
    , Cmd.none
    )



-- UPDATE


type Msg
    = EditText String
    | Add
    | Delete Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditText message ->
            ( { model | message = message }, Cmd.none )

        Add ->
            let
                m =
                    .message model

                ms =
                    .messages model
            in
                ( { model | message = "", messages = m :: ms }, Cmd.none )

        Delete index ->
            let
                ms =
                    .messages model
            in
                ( { model | messages = deleteAtIndex index ms }, Cmd.none )


deleteAtIndex i xs =
    (List.take i xs) ++ (List.drop (i + 1) xs)



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ textInput model
        , addTodo
        , todos (.messages model)
        ]


textInput : Model -> Html Msg
textInput model =
    input [ placeholder "enter text", onInput EditText, style (List.append inputStyle [ ( "width", "500px" ) ]), value model.message ] []


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
        , button [ onClick (Delete i), style[("float","right")] ] [ text ("delete" ++ (toString i)) ]
        , Html.br [] []
        ]



-- SUBS


subscriptions model =
    Sub.none
