module Ui.Chip exposing (..)

import Html exposing (Html, div, input, text, Attribute, span, button, li, ul)
import Html.Attributes as Attr exposing (placeholder, checked, type_)
import Html.Events exposing (onClick, onInput)
import Icons.Icon as Icon
import Ui.ChipCss exposing (..)


{ id, class, classList } =
    chipNamespace


view : { label : String, value : String } -> (String -> msg) -> Html msg
view item toMsg =
    div [ class [ Container ] ]
        [ span [ class [ SpanText ] ] [ text item.label ]
        , button [ class [ Button ], onClick (toMsg item.value) ]
            [ Icon.view "cancel"
            ]
        ]
