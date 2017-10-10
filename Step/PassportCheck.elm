module Step.PassportCheck exposing (view)

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

        dateOfBirthConfig =
            { tfConfig | labelText = Just "Дата рождения", readonly = True }

        passportSeriesConfig =
            { tfConfig | labelText = Just "Серия", readonly = True }

        passportNumberConfig =
            { tfConfig | labelText = Just "Номер", readonly = True }

        issuedAtConfig =
            { tfConfig | labelText = Just "Дата выдачи", readonly = True }
    in
        styled Html.div
            [ cs "block" ]
            [ styled div [ Typography.headline ] [ text "Проверка паспорта" ]
            , div []
                [ Textfield.viewReadonly (Just "4212")
                    Textfield.defaultModel
                    passportSeriesConfig
                , Textfield.viewReadonly (Just "122312")
                    Textfield.defaultModel
                    passportNumberConfig
                , Textfield.viewReadonly (Just "12.12.2012")
                    Textfield.defaultModel
                    issuedAtConfig
                , Textfield.viewReadonly (Just "12.12.2012")
                    Textfield.defaultModel
                    dateOfBirthConfig
                ]
            ]
