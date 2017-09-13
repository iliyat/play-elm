module Ui.Chip exposing (..)

import Html exposing (Html, div, input, text, Attribute, span, button, li, ul)
import Html.Attributes as Attr exposing (placeholder, checked, type_)
import Html.Events exposing (onClick, onInput)
import Icons.Icon as Icon
import Ui.ChipCss exposing (..)


{ id, class, classList } =
    chipNamespace


view : Html msg
view =
    div [ class [ Container ] ]
        [ span [ class [ SpanText ] ] [ text "test" ]
        , button [ class [ Button ] ]
            [ Icon.view "cancel"
            ]
        ]
