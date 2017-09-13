module Ui.ChipCss exposing (..)

import Css exposing (..)
import Css.Namespace exposing (namespace)
import Html.CssHelpers exposing (withNamespace)


type CssClasses
    = Container
    | SpanText
    | Button


chipNamespace =
    withNamespace "chip"


css =
    (stylesheet << namespace chipNamespace.name)
        [ class Container
            [ border (px 10)
            , boxSizing borderBox
            , display inlineBlock
            , cursor default
            , backgroundColor (rgb 237 239 241)
            , borderRadius (px 16)
            , padding2 (px 0) (px 12)
            , paddingRight (px 4)
            , marginRight (px 4)
            ]
        , class SpanText
            [ color (rgb 120 144 156)
            , fontSize (px 14)
            , lineHeight (px 32)
            , property "font-family" "\"Roboto\""
            ]
        , class Button
            [ height (px 24)
            , width (px 24)
            , backgroundColor (rgba 0 0 0 0)
            , opacity (num 0.54)
            , cursor pointer
            , padding (px 0)
            , margin4 (px 0) (px 0) (px 3) (px 4)
            , fontSize (px 13)
            , textDecoration none
            , border zero
            , outline none
            , verticalAlign middle
            , display inlineBlock
            ]
        ]
