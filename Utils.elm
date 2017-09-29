module Utils exposing (rusLocale, pluralize, Plural, Plural(..))

import FormatNumber.Locales exposing (Locale)


type Plural
    = Plural String String String


rusLocale : Locale
rusLocale =
    Locale 0 " " "." "-" ""


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
