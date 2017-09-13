module Views exposing (search, menu, Item(..), Config)

import Html exposing (Html, div, input, text, Attribute, span, button, li, ul)
import Html.Attributes as Attr exposing (placeholder, checked, type_)
import Html.Events exposing (onClick, onInput)
import Classes exposing (..)
import Icons.Icon as Icon
import Ui.Chip as Chip
import Menu exposing (Common, onInputChange)


-- VIEW


{ id, class, classList } =
    searchBoxNamespace


type alias Config msg =
    { onSearchChange : Common -> msg
    , onTagDelete : String -> msg
    , onMenuClick : String -> msg
    , open : Bool
    , tags : List { label : String, value : String }
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
            , class
                [ Menu
                , when (not isVisible) Hidden
                ]
            ]
            [ div
                [ class
                    [ MenuInner1
                    , when (not isVisible) MenuInner1Hidden
                    ]
                ]
                [ div
                    [ class
                        [ MenuInner2
                        , when (not isVisible)
                            MenuInner2Hidden
                        ]
                    ]
                    [ ul [ Attr.class "mdc-simple-menu__items mdc-list" ]
                        (List.map mapper items)
                    ]
                ]
            ]


search : Config msg -> List (Item String String) -> Html msg
search { onTagDelete, onSearchChange, onMenuClick, tags, open } menuItems =
    let
        inpt_ =
            div [ class [ InputContainer ] ]
                [ input [ Menu.onInputChange onSearchChange, class [ Input ], type_ "text", placeholder "поиск" ] []
                , menu open menuItems onMenuClick
                ]

        chips =
            List.map (\tag -> Chip.view tag onTagDelete) tags
    in
        div [ class [ Container ] ]
            [ div [ class [ Search ] ]
                [ div [ class [ SearchBlock ] ]
                    [ div [ class [ SearchIcon ] ] [ Icon.view "search" ]
                    , div [ class [ TagsBlock ] ]
                        (chips ++ [ inpt_ ])
                    ]
                , div [ class [ Block ] ] [ Icon.view "close" ]
                ]
            ]
