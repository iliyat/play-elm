module Views exposing (search, menu, Item(..), Config, table)

import Html exposing (Html, div, input, text, Attribute, span, button, li, ul)
import Html.Attributes as Attr exposing (placeholder, checked, type_)
import Html.Events exposing (onClick, onInput)
import Classes exposing (..)
import Icons.Icon as Icon
import Ui.Chip as Chip
import AutocompleteMenu exposing (Common, onInputChange)
import Table


-- VIEW


{ id, class, classList } =
    searchBoxNamespace


type alias Config msg =
    { onSearchChange : Common -> msg
    , onTagDelete : { value : String, label : String } -> msg
    , onMenuClick : String -> msg
    , onCloseClick : msg
    , onOpenSearchClick : msg
    , open : Bool
    , selectedTags : List { label : String, value : String }
    , searchAvailable : Bool
    }


type Item v l
    = ValueItem v l
    | Divider


when : Bool -> (CssClasses -> CssClasses)
when guard cl =
    if guard then
        cl
    else
        Empty


menu : Bool -> List (Item String String) -> (String -> msg) -> Html msg
menu isVisible items toMsg =
    let
        mapper item =
            case item of
                ValueItem v l ->
                    li
                        [ Attr.class "mdc-list-item"
                        , class [ ListItem ]
                        , onClick (toMsg v)
                        ]
                        [ text l ]

                Divider ->
                    li [ Attr.class "mdc-list-divider", class [ ListItemDivider ] ] []
    in
        div
            [ Attr.class "mdc-simple-menu mdc-simple-menu--open"
            , class [ Menu, when (not isVisible || (List.length items == 0)) Hidden ]
            ]
            [ div
                [ class [ MenuInner1, when (not isVisible) MenuInner1Hidden ] ]
                [ div
                    [ class [ MenuInner2, when (not isVisible) MenuInner2Hidden ] ]
                    [ ul [ Attr.class "mdc-simple-menu__items mdc-list" ]
                        (List.map mapper items)
                    ]
                ]
            ]


search : Config msg -> List (Item String String) -> Html msg
search { onTagDelete, onSearchChange, onMenuClick, onOpenSearchClick, onCloseClick, selectedTags, open, searchAvailable } menuItems =
    let
        inpt_ =
            div [ class [ InputContainer ] ]
                [ input
                    [ Menu.onInputChange onSearchChange
                    , class [ Input ]
                    , type_ "text"
                    , placeholder "поиск"
                    , id "input-search"
                    ]
                    []
                , menu open menuItems onMenuClick
                ]

        chips =
            List.map (\tag -> Chip.view tag onTagDelete) selectedTags

        content =
            case searchAvailable of
                True ->
                    [ div [ class [ SearchBlock ] ]
                        [ div [ class [ SearchIcon ] ] [ Icon.view "search" [] ]
                        , div [ class [ TagsBlock ] ]
                            (chips ++ [ inpt_ ])
                        ]
                    , div [ class [ Block ] ] [ Icon.asButton "close" [ onClick onCloseClick ] ]
                    ]

                False ->
                    [ div [ class [ SearchBlock ] ]
                        [ div [ class [ SearchIcon ] ] [ Icon.view "search" [] ]
                        ]
                    , div [ class [ Block ] ]
                        [ Icon.asButton "filter_list"
                            [ onClick
                                onOpenSearchClick
                            ]
                        ]
                    ]
    in
        div [ class [ Container ] ]
            [ div [ class [ Search ] ] content ]


table : Table.Config data msg -> Table.State -> List data -> Int -> Html msg
table tableConfig tableState list limit =
    div [ class [ Container ] ]
        [ div [ class [ TitleContainer ] ] [ text "Заявки (3)" ]
        , Table.view tableConfig tableState list 20
        ]
