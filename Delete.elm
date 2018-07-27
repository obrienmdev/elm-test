module Delete exposing (delete)

import Model exposing (Model)


delete : Int -> Model -> Model
delete index model =
    let
        ms =
            .messages model
    in
        { model | messages = deleteAtIndex index ms }


deleteAtIndex i xs =
    (List.take i xs) ++ (List.drop (i + 1) xs)
