module Step.PassportCheck exposing (view)

import Html exposing (Html, div, h1, text, p, span)
import Html.Attributes as Attrs
import Ui.Options as Options exposing (styled, cs, css)
import Ui.Typography as Typography


view : Html Never
view =
    styled Html.div
        [ cs "block" ]
        [ styled div [ Typography.headline ] [ text "Проверка паспорта" ]
        ]
