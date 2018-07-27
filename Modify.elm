module Modify exposing (begin, end, cancel)

import Model exposing (Model)
import Array
import Todo exposing (Todo)


begin : Model -> Int -> Model
begin model index =
    let
        t =
            atIndex (.messages model) index
    in
        case t of
            Nothing ->
                model

            Just t ->
                let
                    modify t =
                        { t | selected = True }

                    m =
                        (setTodo model index modify)
                in
                    { m | tempMessage = .message t }


cancel : Model -> Int -> Model
cancel model index =
    let
        modify t =
            { t | selected = False }
    in
        setTodo model index modify


end : Model -> Int -> String -> Model
end model index message =
    let
        modify t =
            { message = message, selected = False }
    in
        setTodo model index modify


setTodo : Model -> Int -> (Todo -> Todo) -> Model
setTodo model index modify =
    let
        m =
            .messages model

        t =
            atIndex m index
    in
        case t of
            Nothing ->
                model

            Just t ->
                { model | messages = setAtIndex m index (modify t) }


atIndex : List a -> Int -> Maybe a
atIndex xs i =
    Array.fromList xs
        |> Array.get i


setAtIndex : List a -> Int -> a -> List a
setAtIndex xs i x =
    (List.take i xs) ++ [ x ] ++ (List.drop (i + 1) xs)
