module SliderCss exposing (CssClasses(..), sliderNamespace, css)

import Css exposing (..)
import Css.Namespace exposing (namespace)
import Html.CssHelpers exposing (withNamespace)


sliderNamespace =
    withNamespace "slider"


type CssClasses
    = LabelsContainer
    | Label


css =
    (stylesheet << namespace "slider")
        [ class LabelsContainer
            [ displayFlex
            , justifyContent spaceBetween
            , position relative
            , bottom (px 15)
            ]
        , class Label
            [ color (hex "#afbec5")
            , property "font-family" "\"Roboto\""
            , fontSize (px 14)
            ]
        ]
