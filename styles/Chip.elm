module Chip exposing (..)

import Html exposing (Html, div, input, text, Attribute, span, button, li, ul)
import Html.Attributes as Attr exposing (placeholder, checked, type_)
import Html.Events exposing (onClick, onInput)
import Icons.Icon as Icon


view : Html msg
view =
    div []
        [ span [] []
        , Icon.view "close" []
        ]
