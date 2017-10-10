module Step.Conditions exposing (view)

import Html exposing (Html, div, h1, text, p, span)
import Html.Attributes as Attrs
import Ui.Options as Options exposing (styled, cs, css)
import Ui.Typography as Typography
import Ui.Textfield as Textfield


view : Html Never
view =
    let
        tfConfig =
            Textfield.defaultConfig

        tfModel =
            Textfield.defaultModel

        status =
            { tfConfig
                | labelText = Just "Одобрено"
                , extraInside = Just "₽"
                , asTitle = True
            }

        period =
            { tfConfig
                | labelText = Just "Период"
                , asTitle = True
            }
    in
        styled Html.div
            [ cs "block" ]
            [ styled div [ Typography.headline ] [ text "Условия займа" ]
            , div []
                [ Textfield.viewReadonly (Just "10000")
                    tfModel
                    status
                , Textfield.viewReadonly (Just "14 дней")
                    tfModel
                    period
                ]
            ]
