module Step.Conditions.Info exposing (view)

import Html exposing (Html, div, h1, text, p, span)
import Html.Attributes as Attrs exposing (style)
import Ui.Options as Options exposing (styled, cs, css, when)
import Ui.Textfield as Textfield


view : Html Never
view =
    let
        tfConfig =
            Textfield.defaultConfig

        tfModel =
            Textfield.defaultModel

        amountConfig =
            { tfConfig
                | labelText = Just "Одобрено"
                , extraInside = Just "₽"
                , asTitle = True
                , readonly = True
            }

        daysConfig =
            { tfConfig
                | labelText = Just "Срок"
                , asTitle = True
                , readonly = True
            }

        payAt =
            { tfConfig
                | labelText = Just "Дата погашения"
                , asTitle = True
                , readonly = True
                , fullWidth = True
            }

        returnConfig =
            { tfConfig
                | labelText = Just "К возврату"
                , extraInside = Just "₽"
                , asTitle = True
                , readonly = True
            }

        percentConfig =
            { tfConfig
                | labelText = Just "В т.ч. процент по займу"
                , extraInside = Just "₽"
                , asTitle = True
                , readonly = True
            }

        promoConfig =
            { tfConfig
                | labelText = Just "Акция"
                , asTitle = True
                , readonly = True
            }

        textfield value config =
            Textfield.viewReadonly (Just value) tfModel config
    in
        div [ Attrs.class "ui-flex" ]
            [ textfield "10000" amountConfig
            , textfield "14 дней" daysConfig
            , styled div
                [ css "width" "300px" ]
                [ textfield "12 сентября 2017" payAt
                ]
            , textfield "13 212" returnConfig
            , textfield "13 212" percentConfig
            , Textfield.viewReadonly (Just "lastchance") tfModel promoConfig
            ]
