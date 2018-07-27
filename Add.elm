module Add exposing (add)

import Model exposing (Model)
import Todo exposing (Todo)


add : Model -> Model
add model =
    let
        m =
            .message model

        t =
            { message = m, selected = False }

        ms =
            .messages model
    in
        if (String.length m > 0) then
            { model | message = "", messages = t :: ms }
        else
            model
