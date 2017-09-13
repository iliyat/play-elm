module MyCss exposing (..)

import Css exposing (..)
import Css.Namespace exposing (namespace)
import Classes exposing (..)


css =
    (stylesheet << namespace searchBoxNamespace.name)
        [ class Container
            [ backgroundColor (rgb 255 255 255)
            , boxShadow5 (px 0) (px 0) (px 2) (px 0) (rgba 52 62 77 0.24)
            , borderRadius (px 2)
            ]
        , class Search
            [ backgroundColor (hex "#fff")
            , borderRadius (px 2)
            , displayFlex
            , justifyContent spaceBetween
            , padding2 (px 8) (px 4)
            , alignItems baseline
            ]
        , class SearchBlock
            [ displayFlex
            , alignItems flexStart
            ]
        , class SearchIcon
            [ minWidth (px 24)
            , alignItems flexStart
            , padding4 (px 12) (px 16) (px 0) (px 22)
            ]
        , class TagsBlock
            [ alignItems baseline
            , displayFlex
            , flexWrap wrap
            ]
        , class Block
            [ alignItems center
            , displayFlex
            , paddingRight (px 12)
            ]
        , class InputContainer
            [ position relative
            ]
        , class Input
            [ outline none
            , borderStyle none
            , property "-webkit-appearance" "textfield"
            , lineHeight (px 24)
            , height (px 48)
            , fontSize (px 16)
            ]
        , class Menu
            [ display block |> important
            , position fixed |> important
            , top (px 50) |> important
            ]
        , class ListItem
            [ height (px 32) |> important
            , fontSize (px 14) |> important
            , hover [ backgroundColor (hex "#eee") ]
            ]
        , class ListItemDivider
            [ margin2 (px 8) (px 0) |> important
            ]
        , class Empty
            []
        , class Hidden
            [ opacity (num 0) |> important
            , position relative
            , property "z-index" "-100500" |> important
            ]
        ]
