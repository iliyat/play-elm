module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events
import Dict exposing (Dict)
import Date exposing (Date, Month(..))
import Ui.Slider as Slider
import Ui.Textfield as Textfield
import Ui.SliderWithTextfield as SliderWithTextfield
import Ui.RadioButton as RadioButton
import Ui.Ripple as Ripple
import Utils.General exposing (..)
import Ui.Elevation as Elevation
import Ui.Typography as Typography
import Ui.Options as Options exposing (styled, cs, css, when)
import Ui.DatePicker as DatePicker
    exposing
        ( defaultSettings
        , DateEvent(..)
        , moreOrLess
        )
import Date.Extra as Date
import Task


type alias Model =
    { textfield : Textfield.Model
    , sumSliderModel : SliderWithTextfield.Model
    , periodSliderModel : SliderWithTextfield.Model
    , sumInputText : Maybe String
    , periodInputText : Maybe String
    , datePicker : DatePicker.DatePicker
    , date : Maybe Date
    , radios : Dict String String
    , radioModel1 : RadioButton.Model
    , radioModel2 : RadioButton.Model
    , ripple : Ripple.Model
    , textInput : Maybe String
    , forPay : Textfield.Model
    , forPayTextInput : Maybe String
    , percent : Textfield.Model
    , percentTextInput : Maybe String
    , perDayPercent : Textfield.Model
    , perDayPercentTextInput : Maybe String
    , perDayAmount : Textfield.Model
    , perDayAmountTextInput : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    let
        ( dp, _ ) =
            DatePicker.init
    in
        { textfield = Textfield.defaultModel
        , textInput = Nothing
        , sumSliderModel = SliderWithTextfield.defaultModel
        , periodSliderModel = SliderWithTextfield.defaultModel
        , sumInputText = Just "2000"
        , periodInputText = Just "7"
        , datePicker = dp
        , date =
            Just <| Date.fromParts 1992 Feb 21 0 0 0 0
        , radios = Dict.fromList []
        , radioModel1 = RadioButton.defaultModel
        , radioModel2 = RadioButton.defaultModel
        , ripple = Ripple.defaultModel
        , forPay = Textfield.defaultModel
        , percent = Textfield.defaultModel
        , perDayPercent = Textfield.defaultModel
        , perDayAmount = Textfield.defaultModel
        , forPayTextInput = Nothing
        , percentTextInput = Nothing
        , perDayPercentTextInput = Nothing
        , perDayAmountTextInput = Nothing
        }
            ! [ Task.perform CurrentDate Date.now ]


type Msg
    = Open
    | SumSliderWithTextfieldMsg SliderWithTextfield.Msg
    | PeriodSliderWithTextfieldMsg SliderWithTextfield.Msg
    | TextfieldMsg Textfield.Msg
    | DatePickerMsg DatePicker.Msg
    | Select Int
    | OnRadioClick String String
    | RadioButtonMsg1 RadioButton.Msg
    | RadioButtonMsg2 RadioButton.Msg
    | RippleMsg Ripple.Msg
    | CurrentDate Date


swtConf1 : SliderWithTextfield.Config
swtConf1 =
    let
        slider =
            Slider.defaultConfig

        textfield =
            Textfield.defaultConfig
    in
        { sliderConfig =
            { slider
                | value = 2000
                , min = 2000
                , max = 10000
                , steps = 1000
            }
        , textfieldConfig =
            { textfield
                | defaultValue = Just "2000"
                , asTitle = True
                , numbered = True
                , extra = Just "₽"
                , fullWidth = True
                , labelText = Just "Сумма"
            }
        , extraStatic = Just "₽"
        , extraPlural = Nothing
        }


swtConf2 : SliderWithTextfield.Config
swtConf2 =
    let
        slider =
            Slider.defaultConfig

        textfield =
            Textfield.defaultConfig
    in
        { sliderConfig =
            { slider
                | steps = 1
            }
        , textfieldConfig =
            { textfield
                | defaultValue = Just "7"
                , asTitle = True
                , numbered = True
                , plural = Just (Plural "день" "дня" "дней")
                , fullWidth = True
                , labelText = Just "Срок"
            }
        , extraPlural = Just (Plural "день" "дня" "дней")
        , extraStatic = Nothing
        }


textfieldConfig : Textfield.Config
textfieldConfig =
    let
        dc =
            Textfield.defaultConfig
    in
        { dc
            | readonly = False
            , labelText = Just "Промокод"
        }


datePickerConfig : DatePicker.Settings
datePickerConfig =
    DatePicker.withLabel "Дата погашения"


isPrimarySelected : Model -> Bool
isPrimarySelected model =
    let
        isSelected isDef group name =
            Dict.get group model.radios
                |> Maybe.map ((==) name)
                |> Maybe.withDefault isDef
    in
        isSelected True "clientGroup" "primary"


currentSumSliderConfig : Bool -> Maybe String -> SliderWithTextfield.Config
currentSumSliderConfig isPrimary inputText =
    let
        inputOriginalText =
            String.toInt (inputText |> Maybe.withDefault "0")
                |> Result.withDefault 0

        floatValue =
            toFloat inputOriginalText

        primaryConfig =
            SliderWithTextfield.withLimits1 swtConf1 floatValue 2000 10000 1000

        secondaryConfig =
            SliderWithTextfield.withLimits1 swtConf1 floatValue 2000 30000 1000
    in
        if isPrimary then
            primaryConfig
        else
            secondaryConfig


currentPeriodSliderConfig : Maybe String -> Maybe String -> SliderWithTextfield.Config
currentPeriodSliderConfig sumInputText_ periodInputText_ =
    let
        sumInputText =
            String.toInt (sumInputText_ |> Maybe.withDefault "0")
                |> Result.withDefault 0

        periodInputText =
            String.toInt (periodInputText_ |> Maybe.withDefault "0")
                |> Result.withDefault 0

        lessThan1000 =
            if sumInputText <= 10000 then
                True
            else
                False

        min =
            if lessThan1000 then
                7
            else
                20

        max =
            if lessThan1000 then
                20
            else
                30

        between value =
            if value < min then
                min
            else if value > max then
                max
            else
                value

        periodValue =
            (between periodInputText)

        config =
            SliderWithTextfield.withLimits1 swtConf2 (toFloat periodValue) min max 1
    in
        config


calculateNewDate : Model -> String -> Date
calculateNewDate model periodInputText =
    let
        daysToAdd_ =
            String.toInt periodInputText |> Result.map (flip (-) 1) |> Result.withDefault 0

        daysToAdd =
            if daysToAdd_ > 100 then
                100
            else
                daysToAdd_
    in
        Date.add Date.Day daysToAdd <| DatePicker.today model.datePicker


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
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

        RippleMsg msg_ ->
            let
                ( new, effects ) =
                    Ripple.update msg_ model.ripple
            in
                ( { model | ripple = new }, effects |> Cmd.map RippleMsg )

        RadioButtonMsg1 msg_ ->
            let
                ( new, radioEffects ) =
                    RadioButton.update RadioButtonMsg1 msg_ model.radioModel1

                r =
                    case new of
                        Nothing ->
                            model.radioModel1

                        Just a ->
                            a
            in
                ( { model | radioModel1 = r }, radioEffects )

        RadioButtonMsg2 msg_ ->
            let
                ( new, radioEffects ) =
                    RadioButton.update RadioButtonMsg2 msg_ model.radioModel2

                r =
                    case new of
                        Nothing ->
                            model.radioModel2

                        Just a ->
                            a
            in
                ( { model | radioModel2 = r }, radioEffects )

        OnRadioClick group value ->
            let
                radio =
                    Dict.get group model.radios
                        |> Maybe.withDefault ""

                isPrimary =
                    if value == "primary" then
                        True
                    else
                        False

                newSumSliderText =
                    (currentSumSliderConfig isPrimary model.sumInputText)
                        |> .sliderConfig
                        |> .value
                        |> toString

                newPeriodInputText =
                    (currentPeriodSliderConfig (Just newSumSliderText) model.periodInputText)
                        |> .sliderConfig
                        |> .value
                        |> toString

                radios =
                    Dict.insert group value model.radios
            in
                { model
                    | radios = radios
                    , sumInputText = Just newSumSliderText
                    , periodInputText = Just newPeriodInputText
                    , date = Just (calculateNewDate model newPeriodInputText)
                }
                    ! []

        DatePickerMsg msg ->
            let
                ( newDatePicker, datePickerFx, dateEvent ) =
                    DatePicker.update
                        model.date
                        datePickerConfig
                        msg
                        model.datePicker

                newDate =
                    case dateEvent of
                        Changed newDate ->
                            newDate

                        _ ->
                            model.date

                min =
                    .min <|
                        .sliderConfig <|
                            (currentPeriodSliderConfig
                                model.sumInputText
                                model.periodInputText
                            )

                max =
                    .max <|
                        .sliderConfig <|
                            (currentPeriodSliderConfig
                                model.sumInputText
                                model.periodInputText
                            )

                today =
                    DatePicker.today model.datePicker

                diff =
                    Date.diff Date.Day
                        today
                        (newDate |> Maybe.withDefault today)
                        |> (+) 2
                        |> toFloat

                newValue =
                    if diff < min then
                        min
                    else if diff > max then
                        max
                    else
                        diff
            in
                { model
                    | date = newDate
                    , datePicker = newDatePicker
                    , periodInputText = Just <| toString newValue
                }
                    ! [ Cmd.map DatePickerMsg datePickerFx ]

        SumSliderWithTextfieldMsg msg_ ->
            let
                ( newSumSliderModel, newSumText ) =
                    SliderWithTextfield.update
                        msg_
                        model.sumSliderModel
                        (currentSumSliderConfig (isPrimarySelected model) model.sumInputText)
                        model.sumInputText

                newPeriodInputText =
                    (currentPeriodSliderConfig newSumText model.periodInputText)
                        |> .sliderConfig
                        |> .value
                        |> toString
            in
                { model
                    | sumSliderModel = newSumSliderModel
                    , sumInputText = newSumText
                    , periodInputText = Just newPeriodInputText
                    , date = Just (calculateNewDate model newPeriodInputText)
                }
                    ! []

        PeriodSliderWithTextfieldMsg msg_ ->
            let
                ( newPeriodSliderModel, newText ) =
                    SliderWithTextfield.update
                        msg_
                        model.periodSliderModel
                        (currentPeriodSliderConfig model.sumInputText
                            model.periodInputText
                        )
                        model.periodInputText
            in
                { model
                    | periodSliderModel = newPeriodSliderModel
                    , date =
                        Just
                            (calculateNewDate model
                                (newText
                                    |> Maybe.withDefault "0"
                                )
                            )
                    , periodInputText =
                        newText
                }
                    ! []

        TextfieldMsg msg_ ->
            let
                ( newTextfieldModel, newText ) =
                    Textfield.externalUpdate
                        msg_
                        model.textfield
                        textfieldConfig
                        model.textInput
            in
                { model | textfield = newTextfieldModel, textInput = newText } ! []

        Select n ->
            model ! []

        Open ->
            model ! []


view : Model -> Html Msg
view model =
    let
        ( rippleOptions, rippleStyles ) =
            Ripple.view False (RippleMsg) model.ripple () ()

        defaultTextfield =
            Textfield.defaultConfig

        forPayConfig =
            { defaultTextfield
                | readonly = True
                , numbered = True
                , extraInside = Just "₽"
                , defaultValue = Just "40000"
                , labelText = Just "К выплате"
                , asTitle = True
            }

        percentConfig =
            { defaultTextfield
                | readonly = True
                , extraInside = Just "₽"
                , numbered = True
                , defaultValue = Just "1896"
                , labelText = Just "Процент по займу"
                , asTitle = True
            }

        perDayPercentConfig =
            { defaultTextfield
                | readonly = True
                , extraInside = Just "%"
                , numbered = True
                , defaultValue = Just "1896"
                , labelText = Just "В среднем в день (%)"
                , asTitle = True
            }

        perDayAmountConfig =
            { defaultTextfield
                | readonly = True
                , extraInside = Just "₽"
                , numbered = True
                , defaultValue = Just "12"
                , labelText = Just "В среднем в день (₽)"
                , asTitle = True
            }
    in
        Html.div []
            [ styled Html.div
                [ cs "main-container"
                , Elevation.z1
                ]
                [ styled div [ Typography.headline ] [ text "Параметры" ]
                , styled div
                    [ cs "params" ]
                    [ div []
                        [ RadioButton.view RadioButtonMsg1
                            model.radioModel1
                            [ Options.onClick (OnRadioClick "clientGroup" "primary")
                            , RadioButton.selected
                                |> when
                                    (isPrimarySelected
                                        model
                                    )
                            , RadioButton.name "primary"
                            ]
                            [ label
                                [ Events.onClick (OnRadioClick "clientGroup" "primary")
                                ]
                                [ text "Первичный клиент" ]
                            ]
                        ]
                    , div []
                        [ RadioButton.view RadioButtonMsg2
                            model.radioModel2
                            [ Options.onClick (OnRadioClick "clientGroup" "secondary")
                            , RadioButton.selected
                                |> when
                                    (not <|
                                        isPrimarySelected
                                            model
                                    )
                            , RadioButton.name "secondary"
                            ]
                            [ label
                                [ Events.onClick (OnRadioClick "clientGroup" "secondary")
                                ]
                                [ text "Повторный клиент" ]
                            ]
                        ]
                    ]
                , styled div
                    [ cs "fields" ]
                    [ SliderWithTextfield.view
                        model.sumInputText
                        model.sumSliderModel
                        (currentSumSliderConfig (isPrimarySelected model)
                            model.sumInputText
                        )
                        |> Html.map SumSliderWithTextfieldMsg
                    , SliderWithTextfield.view
                        model.periodInputText
                        model.periodSliderModel
                        (currentPeriodSliderConfig model.sumInputText
                            model.periodInputText
                        )
                        |> Html.map PeriodSliderWithTextfieldMsg
                    , DatePicker.view
                        model.date
                        datePickerConfig
                        model.datePicker
                        |> Html.map DatePickerMsg
                    ]
                ]
            , styled Html.div
                [ cs "main-container"
                , Elevation.z1
                ]
                [ styled div [ Typography.headline ] [ text "Расчет займа" ]
                , styled div
                    [ cs "loan-wrapper" ]
                    [ div []
                        [ Textfield.view
                            model.forPayTextInput
                            model.forPay
                            forPayConfig
                            |> Html.map TextfieldMsg
                        ]
                    , div []
                        [ Textfield.view
                            model.percentTextInput
                            model.percent
                            percentConfig
                            |> Html.map TextfieldMsg
                        ]
                    , div []
                        [ Textfield.view
                            model.perDayPercentTextInput
                            model.perDayPercent
                            perDayPercentConfig
                            |> Html.map TextfieldMsg
                        ]
                    , div []
                        [ Textfield.view
                            model.perDayAmountTextInput
                            model.perDayAmount
                            perDayAmountConfig
                            |> Html.map TextfieldMsg
                        ]
                    ]
                ]
            , Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "mdc.css" ] []
            , Html.node "script" [ Html.Attributes.src "mdc.js" ] []
            , Html.node "link"
                [ Html.Attributes.rel "stylesheet"
                , Html.Attributes.href "material-components-web.css"
                ]
                []
            , Html.node "link"
                [ Html.Attributes.rel "stylesheet"
                , Html.Attributes.href "https://fonts.googleapis.com/icon?family=Material+Icons"
                ]
                []
            , Html.node "link"
                [ Html.Attributes.rel "stylesheet", Html.Attributes.href "https://fonts.googleapis.com/css?family=Roboto:300,400,500" ]
                []
            , Html.node "link"
                [ Html.Attributes.rel "stylesheet"
                , Html.Attributes.href
                    "slider.css"
                ]
                []
            , Html.node "link"
                [ Html.Attributes.rel "stylesheet"
                , Html.Attributes.href
                    "datepicker.css"
                ]
                []
            , Html.node "link"
                [ Html.Attributes.rel "stylesheet"
                , Html.Attributes.href
                    "main.css"
                ]
                []
            ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map SumSliderWithTextfieldMsg
            (SliderWithTextfield.subscriptions
                model.sumSliderModel
            )
        , Sub.map PeriodSliderWithTextfieldMsg
            (SliderWithTextfield.subscriptions
                model.periodSliderModel
            )
        ]
