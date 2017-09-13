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
                div
                    [ Attr.class "mdc-simple-menu mdc-simple-menu--open"
                    , class
                        [ Menu ]
                    ]
                    [ ul [ Attr.class "mdc-simple-menu__items mdc-list" ]
                        (List.map mapper items)
                    ]

            False ->
                div [] []


search :
    List { label : String, value : String }
    -> (String -> msg)
    ->
        (String
         -> msg
        )
    -> Bool
    -> List (Item String String)
    -> (String -> msg)
    -> Html msg
search tags searchTextChange tagDelete isVisible menuItems menuToMsg =
    let
        inpt_ =
            div [ class [ InputContainer ] ]
                [ input [ onInput searchTextChange, class [ Input ], type_ "text", placeholder "поиск" ] []
                , menu isVisible menuItems menuToMsg
                ]

        chips =
            List.map (\tag -> Chip.view tag tagDelete) tags
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
