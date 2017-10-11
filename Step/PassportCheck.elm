module Step.PassportCheck exposing (view, update, Msg, Model, defaultModel)

import Html exposing (Html, div, h1, text, p, span)
import Html.Attributes as Attrs
import Ui.Options as Options exposing (styled, cs, css, when)
import Ui.Typography as Typography
import Ui.Textfield as Textfield
import Utils.Style exposing (mkClass, mkClassList)
import Icons.Icon as Icon
import Ui.Elevation as Elevation
import Ui.Button as Button


class =
    mkClass "passport-check--"


classList =
    mkClass "passport-check--"


type alias Model =
    { buttonModel : Button.Model
    , formVisible : Bool
    }


defaultModel : Model
defaultModel =
    { buttonModel = Button.defaultModel
    , formVisible = False
    }


type Msg
    = Ripple Button.Msg
    | AddNewPassport


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddNewPassport ->
            { model | formVisible = True } ! []

        Ripple msg_ ->
            let
                ( new, effects ) =
                    Button.update msg_ model.buttonModel
            in
                ( { model | buttonModel = new }, effects |> Cmd.map Ripple )


passportValid : Bool -> Html Msg
passportValid isValid =
    let
        class =
            mkClass "passport-check--validation-"

        classList =
            mkClassList "passport-check--validation-"

        iconName =
            if not isValid then
                "warning"
            else
                "check_circle"

        titleText =
            if isValid then
                "Паспорт действителен"
            else
                "Паспорт  недействителен"

        titleHelpText =
            if isValid then
                "Продолжайте работу с клиентом"
            else
                "Сообщите клиенту об отказе по заявке"
    in
        div [ classList [ ( "wrapper", True ), ( "wrapper--invalid", not isValid ) ] ]
            [ div [ classList [ ( "square", True ), ( "square--invalid", not isValid ) ] ]
                [ div [ class "icon-container" ]
                    [ Icon.view iconName
                        [ Attrs.style
                            [ ( "font-size", "36px" )
                            , ( "color", "white" )
                            ]
                        ]
                    ]
                ]
            , div [ class "info" ]
                [ div
                    [ classList
                        [ ( "title", True )
                        , ( "title--invalid"
                          , not
                                isValid
                          )
                        ]
                    ]
                    [ text titleText ]
                , span [ class "helper-text" ] [ text titleHelpText ]
                ]
            ]


view : Model -> Html Msg
view model =
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

        headlineText =
            if model.formVisible then
                "Текущий паспорт"
            else
                "Проверка паспорта"
    in
        styled div
            [ cs "block", Elevation.z1 ]
            [ styled div [ Typography.headline ] [ text headlineText ]
            , styled div
                [ cs "ui-flex" ]
                [ styled div
                    [ cs "ui-flex" ]
                    [ Textfield.viewReadonly (Just "4212")
                        Textfield.defaultModel
                        passportSeriesConfig
                        |> Html.map never
                    , Textfield.viewReadonly (Just "122312")
                        Textfield.defaultModel
                        passportNumberConfig
                        |> Html.map never
                    , Textfield.viewReadonly (Just "12.12.2012")
                        Textfield.defaultModel
                        issuedAtConfig
                        |> Html.map never
                    , Textfield.viewReadonly (Just "12.12.2012")
                        Textfield.defaultModel
                        dateOfBirthConfig
                        |> Html.map never
                    ]
                , passportValid True
                ]
            , Button.view Ripple
                model.buttonModel
                [ Button.ripple
                , Button.primary
                , Options.onClick AddNewPassport
                ]
                [ text "Добавить новый паспорт" ]
            , styled div
                [ css "display" "none" |> when (not model.formVisible)
                ]
                [ styled div [ Typography.headline ] [ text "Новый паспорт" ]
                ]
            ]
