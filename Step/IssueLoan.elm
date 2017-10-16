module Step.IssueLoan exposing (view)

import Html exposing (Html, div, h1, text, p, span)
import Html.Attributes as Attrs
import Ui.Options as Options exposing (styled, cs, css)
import Ui.Typography as Typography
import Ui.Elevation as Elevation


view : Html Never
view =
    div []
        [ styled Html.div
            [ Elevation.z1, cs "block" ]
            [ styled div [ Typography.headline ] [ text "Выплата ден.средств по Договору №999-99999 завершена" ]
            , p []
                [ text "Поблагодарите клиента за сотрудничество" ]
            ]
        ]
