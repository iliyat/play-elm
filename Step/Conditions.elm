module Step.Conditions exposing (view, Model, Msg, init, update)

import Html exposing (Html, div, h1, text, p, span)
import Ui.Options as Options exposing (styled, cs, css, when)
import Ui.Typography as Typography
import Ui.Textfield as Textfield
import Ui.Button as Button
import Ui.Elevation as Elevation
import Ui.SliderWithTextfield as SliderWithTextfield
import Ui.DatePicker as DatePicker
    exposing
        ( defaultSettings
        , DateEvent(..)
        , moreOrLess
        )
import Date exposing (Date, Month(..))
import Date.Extra as Date
import Task
import LoanDetails
import Step.Conditions.Info as ConditionsInfo


-- ID                              string `gorm:"type:uuid;primary_key;"`
-- ExternalId                      string `gorm:"type:uuid;index;"`
-- NumberPrefix                    uint32 `gorm:"index"`
-- Number                          uint32 `gorm:"index"`
-- RequestAt                       time.Time
-- RequestBy                       *string
-- RequestAmount                   uint64
-- RequestDaysCount                uint32
-- RequestPurpose                  string
-- RequestModelCalculatingDailyId  string `gorm:"type:uuid;index;"`
-- RequestPromoCode                string
-- IsActual                        bool
-- Status                          int32
-- ApprovedAt                      *time.Time
-- ApprovedBy                      *string `gorm:"type:uuid;index;"`
-- ApprovedAmount                  *uint64
-- ApprovedDaysCount               *uint32
-- ApprovedModelCalculatingDailyId *string `gorm:"type:uuid;index;"`
-- TimeZone                        *string
-- RejectReason                    *string
-- RiskClientType                  bool
-- RiskCountry                     bool
-- RiskClientOperations            bool
-- RiskUpdatedAt                   *time.Time
-- CreatedAt                       time.Time
-- UpdatedAt                       time.Time
-- DeletedAt                       *time.Time
-- MODEL --


type alias Model =
    { errors : List String
    , buttonModel : Button.Model
    , showConditionsBlock : Bool
    , sumSliderModel : SliderWithTextfield.Model
    , periodSliderModel : SliderWithTextfield.Model
    , sumInputText : Maybe String
    , periodInputText : Maybe String
    , datePicker : DatePicker.DatePicker
    , date : Maybe Date
    , loanDetailsModel : LoanDetails.Model
    }


init : ( Model, Cmd Msg )
init =
    let
        ( dp, _ ) =
            DatePicker.init
    in
        { errors = []
        , buttonModel = Button.defaultModel
        , showConditionsBlock = False
        , sumSliderModel = SliderWithTextfield.defaultModel
        , periodSliderModel = SliderWithTextfield.defaultModel
        , sumInputText = Just "2000"
        , periodInputText = Just "7"
        , datePicker = dp
        , date =
            Just <| Date.fromParts 1992 Feb 21 0 0 0 0
        , loanDetailsModel = LoanDetails.defaultModel
        }
            ! [ Task.perform CurrentDate Date.now ]


type Msg
    = ChangeConditionsClick
    | Ripple Button.Msg
    | SumSliderWithTextfieldMsg SliderWithTextfield.Msg
    | PeriodSliderWithTextfieldMsg SliderWithTextfield.Msg
    | DatePickerMsg DatePicker.Msg
    | CurrentDate Date
    | LoanDetailsMsg LoanDetails.Msg


sumSliderConfig : Maybe String -> SliderWithTextfield.Config
sumSliderConfig inputText =
    let
        inputOriginalText =
            String.toInt (inputText |> Maybe.withDefault "0")
                |> Result.withDefault 0

        floatValue =
            toFloat inputOriginalText
    in
        SliderWithTextfield.withLimits1 SliderWithTextfield.sumConfig floatValue 2000 30000 1000


periodSliderConfig : Maybe String -> SliderWithTextfield.Config
periodSliderConfig inputText =
    let
        inputOriginalText =
            String.toInt (inputText |> Maybe.withDefault "0")
                |> Result.withDefault 0

        floatValue =
            toFloat inputOriginalText
    in
        SliderWithTextfield.withLimits1 SliderWithTextfield.periodConfig
            floatValue
            10
            30
            1


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CurrentDate today ->
            let
                initDate =
                    (Date.add Date.Day 6 today)
            in
                { model
                    | datePicker = DatePicker.initFromDate initDate today
                    , date = Just initDate
                }
                    ! []

        ChangeConditionsClick ->
            ({ model | showConditionsBlock = True }) ! []

        Ripple msg_ ->
            let
                ( new, effects ) =
                    Button.update msg_ model.buttonModel
            in
                ( { model | buttonModel = new }, effects |> Cmd.map Ripple )

        SumSliderWithTextfieldMsg msg_ ->
            let
                ( newSumSliderModel, newSumText ) =
                    SliderWithTextfield.update
                        msg_
                        model.sumSliderModel
                        (sumSliderConfig model.sumInputText)
                        model.sumInputText
            in
                { model
                    | sumSliderModel = newSumSliderModel
                    , sumInputText = newSumText
                }
                    ! []

        PeriodSliderWithTextfieldMsg msg_ ->
            let
                ( newPeriodSliderModel, newText ) =
                    SliderWithTextfield.update
                        msg_
                        model.periodSliderModel
                        (periodSliderConfig model.periodInputText)
                        model.periodInputText
            in
                { model
                    | periodSliderModel = newPeriodSliderModel
                    , periodInputText =
                        newText
                }
                    ! []

        DatePickerMsg msg_ ->
            let
                ( newDatePicker, datePickerFx, dateEvent ) =
                    DatePicker.update
                        model.date
                        (DatePicker.withLabel "Дата погашения")
                        msg_
                        model.datePicker

                newDate =
                    case dateEvent of
                        Changed newDate ->
                            newDate

                        _ ->
                            model.date
            in
                { model
                    | date = newDate
                    , datePicker = newDatePicker
                }
                    ! [ Cmd.map DatePickerMsg datePickerFx ]

        LoanDetailsMsg msg_ ->
            let
                ( newModel, _ ) =
                    LoanDetails.update msg_ model.loanDetailsModel
            in
                { model | loanDetailsModel = newModel } ! []


view : Model -> Html Msg
view model =
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

        datepickerTextfieldConfig =
            { tfConfig
                | labelText = Just "Дата погашения"
                , asTitle = True
                , width = 368
            }

        loanNumber =
            "435-43313"

        headlineText =
            if model.showConditionsBlock then
                "Старые условия займа"
            else
                "Заявка № " ++ loanNumber

        textfield value config =
            Textfield.viewReadonly (Just value) tfModel config |> Html.map never

        hiddenBlockAttributes =
            [ Elevation.z1
            , cs "block"
            , css "margin-top" "24px"
            , css "display" "none"
                |> when (not model.showConditionsBlock)
            ]
    in
        div []
            [ styled div
                [ Elevation.z1, cs "block" ]
                [ styled div
                    [ css "display" "flex"
                    , css "justify-content" "space-between"
                    ]
                    [ styled div
                        [ Typography.headline, Typography.pad12 ]
                        [ text <| headlineText ]
                    , Button.view Ripple
                        model.buttonModel
                        [ Button.ripple
                        , Button.primary
                        , Options.onClick ChangeConditionsClick
                        , css "display" "none"
                            |> when (model.showConditionsBlock)
                        ]
                        [ text "Изменить условия" ]
                    ]
                , ConditionsInfo.view |> Html.map never
                ]
            , styled div
                hiddenBlockAttributes
                [ div []
                    [ styled div
                        [ Typography.headline, Typography.pad12 ]
                        [ text <| "Изменение условий по заявке № " ++ loanNumber
                        ]
                    , div []
                        [ styled div
                            [ cs "fields" ]
                            [ SliderWithTextfield.view
                                model.sumInputText
                                model.sumSliderModel
                                (sumSliderConfig model.sumInputText)
                                |> Html.map SumSliderWithTextfieldMsg
                            , SliderWithTextfield.view
                                model.periodInputText
                                model.periodSliderModel
                                (periodSliderConfig model.periodInputText)
                                |> Html.map PeriodSliderWithTextfieldMsg
                            , DatePicker.view
                                model.date
                                (DatePicker.withTextfield
                                    datepickerTextfieldConfig
                                )
                                model.datePicker
                                |> Html.map DatePickerMsg
                            ]
                        , styled div
                            [ cs "ui-form-row" ]
                            [ textfield "1000" returnConfig
                            , textfield "12" percentConfig
                            , textfield "12" promoConfig
                            ]
                        ]
                    ]
                ]
            , styled div
                hiddenBlockAttributes
                [ LoanDetails.render model.loanDetailsModel |> Html.map LoanDetailsMsg
                ]
            ]
