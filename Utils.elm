module Utils exposing (..)


type Plural
    = Plural String String String


pluralize : Plural -> Int -> String
pluralize (Plural one two five) count =
    let
        n =
            count % 100

        n10 =
            count % 10
    in
        if n >= 5 && n <= 20 then
            five
        else if n10 == 1 then
            one
        else if n10 >= 2 && n <= 4 then
            two
        else
            five
