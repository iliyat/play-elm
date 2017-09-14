module MyCss exposing (..)

import Css exposing (..)
import Css.Elements exposing (body)
import Css.Namespace exposing (namespace)
import Classes exposing (..)


css =
    (stylesheet << namespace searchBoxNamespace.name)
        [ body
            [ backgroundColor (rgb 237 239 241)
            ]
        , class Container
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
            , minHeight (px 56)
            , maxHeight (px 56)
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
            , property "transform-origin" "left top 0px"
            , transform (scale2 1 1)
            , property "transition" "transform 250ms cubic-bezier(0.23, 1, 0.32, 1) 0ms, opacity 250ms cubic-bezier(0.23, 1, 0.32, 1) 0ms"
            ]
        , class MenuInner1
            [ property "transform-origin" "left top 0px"
            , property "transition" "transform 250ms cubic-bezier(0.23, 1, 0.32, 1) 0ms, opacity 250ms cubic-bezier(0.23, 1, 0.32, 1) 0ms"
            , transform (scaleX 1)
            ]
        , class MenuInner1Hidden
            [ opacity (num 0) |> important
            , transform (scaleX 0)
            ]
        , class MenuInner2
            [ property "transform-origin" "left top 0px"
            , property "transition" "transform 500ms cubic-bezier(0.23, 1, 0.32, 1) 0ms, opacity 500ms cubic-bezier(0.23, 1, 0.32, 1) 0ms"
            , transform (scaleY 1)
            ]
        , class MenuInner2Hidden
            [ opacity (num 0) |> important
            , transform (scaleY 0)
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
            , transform (scale2 0 0)
            ]
        , class TitleContainer
            [ fontSize (px 20)
            , padding2 (px 20) (px 24)
            ]
        ]
