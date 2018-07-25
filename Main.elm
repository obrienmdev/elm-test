import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (placeholder, style)
import Html.Events exposing (onClick, onInput)
import Http 

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions}

-- MODEL

type alias Model = 
  {
    content: String
  }

init : (Model, Cmd Msg)
init =
  ({content = ""}, Cmd.none)


-- UPDATE

type Msg = Increment | Decrement | Edit String 
  | Fetch
  | Receive (Result Http.Error String)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Edit newContent ->
      ({ model | content = newContent }, Cmd.none)

    Increment ->
      ({ model | content = (String.reverse (.content model)) }, Cmd.none)

    Decrement ->
      ({ model | content = "2" }, Cmd.none)

    Fetch ->
      (model, fetch ("/" ++ model.content))

    Receive (Ok data) ->
      ({ model | content = data }, Cmd.none)

    Receive (Err err) ->
      ({ model | content = toString err  }, Cmd.none)

fetch : String -> Cmd Msg
fetch url =
  let request = Http.getString url
  in
    Http.send Receive request

-- VIEW

view : Model -> Html Msg
view model =
  div [] [
    input [ placeholder "enter text", onInput Edit ] [],
    Html.br [] [],
    button [ onClick Increment ] [ text "button" ],
    button [ onClick Fetch ] [ text "fetch!" ],
    display model
  ]

col model =
  if String.length (.content model) > 5 then "red" else "green"

display : Model -> Html Msg
display model =
  let colour = col model in
  div [ style [("color",colour)]] [ text (.content model) ]


subscriptions model = Sub.none

