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

        passportConfig =
            { tfConfig | labelText = Just "Паспорт", readonly = True }

        phoneConfig =
            { tfConfig | labelText = Just "Моб. телефон", readonly = True }

        loanNumberConfig =
            { tfConfig | labelText = Just "Номер займа", readonly = True }
    in
        styled Html.div
            [ cs "block" ]
            [ styled div [ Typography.headline ] [ text "Проверка паспорта" ]

            -- , div []
            --     [ Textfield.view (Just "24.05.1990")
            --         Textfield.defaultModel
            --         dateOfBirthConfig |> Html.map (mapper)
            --     ]
            ]
