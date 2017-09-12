module Roles exposing (main)

import Html exposing (div, span, text)
import Html.Attributes
import MyCss
import Css exposing (backgroundColor, rgb)


styles =
    Css.asPairs >> Html.Attributes.style


main : Html.Html msg
main =
    Html.div [ styles [ backgroundColor (rgb 90 90 90) ] ]
        [ span [] [ text "test" ]
        ]
