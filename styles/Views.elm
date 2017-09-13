module Views exposing (search, menu, Item(..))

import Html exposing (Html, div, input, text, Attribute, span, button, li, ul)
import Html.Attributes as Attr exposing (placeholder, checked, type_)
import Html.Events exposing (onClick, onInput)
import Classes exposing (..)
import Icons.Icon as Icon
import Ui.Chip as Chip


-- VIEW


{ id, class, classList } =
    searchBoxNamespace


type Item v l
    = ValueItem v l
    | Divider


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
        case isVisible of
            True ->
                div [ Attr.class "mdc-simple-menu mdc-simple-menu--open" ]
                    [ ul [ Attr.class "mdc-simple-menu__items mdc-list" ]
                        (List.map mapper items)
                    ]

            False ->
                div [] []


search : (String -> msg) -> Html msg
search toMsg =
    div [ class [ Container ] ]
        [ div [ class [ Search ] ]
            [ div [ class [ SearchBlock ] ]
                [ div [ class [ SearchIcon ] ] [ Icon.view "search" ]
                , div [ class [ TagsBlock ] ]
                    [ Chip.view
                    , input [ onInput toMsg, class [ Input ], type_ "text", placeholder "поиск" ] []
                    ]
                ]
            , div [ class [ Block ] ] [ Icon.view "close" ]
            ]
        ]
