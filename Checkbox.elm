module Checkbox exposing (..)

import Html exposing (Html, div, input, text)
import Html.Attributes exposing (placeholder, class, checked, type_)
import Svg exposing (svg, path)
import Svg.Attributes exposing (fill, stroke, d, viewBox, width, height)


view : Bool -> Html msg
view selected =
    div [ class "mdc-checkbox" ]
        [ input [ type_ "checkbox", class "mdc-checkbox__native-control", checked selected ] []
        , div [ class "mdc-checkbox__background" ] [ icon ]
        ]


icon : Html msg
icon =
    svg
        [ viewBox "0 0 24 24", Svg.Attributes.class "mdc-checkbox__checkmark" ]
        [ path [ Svg.Attributes.class "mdc-checkbox__checkmark__path", fill "none", stroke "white", d "M1.73,12.91 8.1,19.28 22.79,4.59" ] []
        ]
