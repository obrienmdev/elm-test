module Add exposing (add)

import Model exposing (Model)


add : Model -> Model
add model =
    let
        m =
            .message model

        ms =
            .messages model
    in
        if (String.length m > 0) then
            { model | message = "", messages = m :: ms }
        else
            model
