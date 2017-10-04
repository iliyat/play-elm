module Consultation exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events
import Dict exposing (Dict)
import Date exposing (Date, Month(..))
import Ui.Slider as Slider
import Ui.Textfield as Textfield
import Ui.TextfieldNumber as TextfieldNumber
import Ui.SliderWithTextfield as SliderWithTextfield
import Ui.RadioButton as RadioButton
import Ui.Ripple as Ripple
import Utils.General exposing (..)
import Ui.Elevation as Elevation
import Ui.Typography as Typography
import Ui.Internal.Textfield as InternalTextfield
import Ui.Internal.Slider as InternalSlider
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
    , sliderWithTextfield1 : SliderWithTextfield.Model
    , sliderWithTextfield2 : SliderWithTextfield.Model
    , sumInputText : Maybe String
    , periodInputText : Maybe String
    , datePicker : DatePicker.DatePicker
    , date : Maybe Date
    , radios : Dict String String
    , radioModel1 : RadioButton.Model
    , radioModel2 : RadioButton.Model
    , ripple : Ripple.Model
    , textInput : Maybe String
    , forPay : TextfieldNumber.Model
    , forPayTextInput : Maybe Int
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
        , sliderWithTextfield1 = SliderWithTextfield.defaultModel
        , sliderWithTextfield2 = SliderWithTextfield.defaultModel
        , sumInputText = Just "2000"
        , periodInputText = Just "7"
        , datePicker = dp
        , date =
            Just <| Date.fromParts 1992 Feb 21 0 0 0 0
        , radios = Dict.fromList []
        , radioModel1 = RadioButton.defaultModel
        , radioModel2 = RadioButton.defaultModel
        , ripple = Ripple.defaultModel
        , forPay = TextfieldNumber.defaultModel
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
    | SliderWithTextfieldMsg1 SliderWithTextfield.Msg
    | SliderWithTextfieldMsg2 SliderWithTextfield.Msg
    | TextfieldMsg Textfield.Msg
    | ForPayMsg TextfieldNumber.Msg
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


sumSliderPrimaryConfig : SliderWithTextfield.Config
sumSliderPrimaryConfig =
    SliderWithTextfield.withLimits swtConf1 2000 10000 1000


sumSliderSecondaryConfig : SliderWithTextfield.Config
sumSliderSecondaryConfig =
    SliderWithTextfield.withLimits swtConf1 2000 30000 1000


periodSliderConfig : SliderWithTextfield.Config
periodSliderConfig =
    SliderWithTextfield.withLimits swtConf2 7 20 1


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
                | value = 7
                , min = 7
                , max = 20
                , steps = 1
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


currentSliderConfig : Model -> SliderWithTextfield.Config
currentSliderConfig model =
    if isPrimarySelected model then
        sumSliderPrimaryConfig
    else
        sumSliderSecondaryConfig


calculatePeriodSliderConfig : Model -> SliderWithTextfield.Config
calculatePeriodSliderConfig model =
    let
        sum =
            String.toInt (model.sumInputText |> Maybe.withDefault "0")
                |> Result.withDefault 0

        lessThan1000 =
            if sum <= 10000 then
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
    in
        SliderWithTextfield.withLimits swtConf2 min max 1


updatePeriodSliderAndDatepicker :
    Model
    -> SliderWithTextfield.Msg
    -> ( SliderWithTextfield.Model, DatePicker.DatePicker, Maybe Date, Maybe String )
updatePeriodSliderAndDatepicker model msg_ =
    let
        ( newModel, newText ) =
            SliderWithTextfield.update
                msg_
                model.sliderWithTextfield2
                (calculatePeriodSliderConfig model)
                model.periodInputText

        newTextString =
            newText |> Maybe.withDefault ""

        daysToAdd_ =
            String.toInt newTextString |> Result.map (flip (-) 1) |> Result.withDefault 0

        daysToAdd =
            if daysToAdd_ > 100 then
                100
            else
                daysToAdd_

        dateForDatepicker =
            Date.add Date.Day daysToAdd <| DatePicker.today model.datePicker

        ( newDatePicker, newDate ) =
            let
                ( newDatePicker_, _, dateEvent ) =
                    DatePicker.update datePickerConfig
                        (DatePicker.Pick (Just dateForDatepicker))
                        model.datePicker

                newDate_ =
                    case dateEvent of
                        Changed newDate_ ->
                            newDate_

                        _ ->
                            model.date
            in
                case msg_ of
                    SliderWithTextfield.TextfieldMsg (InternalTextfield.Input _) ->
                        ( newDatePicker_, newDate_ )

                    SliderWithTextfield.TextfieldMsg (InternalTextfield.SetValue _) ->
                        ( newDatePicker_, newDate_ )

                    SliderWithTextfield.TextfieldMsg InternalTextfield.Blur ->
                        ( newDatePicker_, newDate_ )

                    SliderWithTextfield.SliderMsg (InternalSlider.MouseDrag _) ->
                        ( newDatePicker_, newDate_ )

                    _ ->
                        ( model.datePicker, model.date )
    in
        ( newModel, newDatePicker, newDate, newText )


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

                checkLimit stringValue sliderConfig =
                    let
                        min =
                            .min <| .sliderConfig <| sliderConfig

                        max =
                            .max <| .sliderConfig <| sliderConfig

                        v =
                            toFloat <| (String.toInt (stringValue |> Maybe.withDefault "0") |> Result.withDefault 0)
                    in
                        if v > max then
                            Just <| toString max
                        else if v < min then
                            Just <| toString min
                        else
                            stringValue

                radios =
                    Dict.insert group value model.radios
            in
                case value of
                    "primary" ->
                        let
                            newSumInputText =
                                checkLimit model.sumInputText sumSliderPrimaryConfig

                            newPeriodInputText =
                                checkLimit model.periodInputText periodSliderConfig

                            ( newSumSliderModel, _ ) =
                                SliderWithTextfield.update
                                    (SliderWithTextfield.TextfieldMsg
                                        (InternalTextfield.Input (newSumInputText |> Maybe.withDefault "0"))
                                    )
                                    model.sliderWithTextfield1
                                    sumSliderPrimaryConfig
                                    model.sumInputText

                            ( newPeriodSliderModel, newDatePicker, newDate, newText ) =
                                updatePeriodSliderAndDatepicker model
                                    (SliderWithTextfield.TextfieldMsg
                                        (InternalTextfield.Input (newPeriodInputText |> Maybe.withDefault "0"))
                                    )
                        in
                            { model
                                | radios = radios
                                , sliderWithTextfield1 = newSumSliderModel
                                , sliderWithTextfield2 = newPeriodSliderModel
                                , sumInputText = newSumInputText
                                , datePicker = newDatePicker
                                , date = newDate
                            }
                                ! []

                    _ ->
                        let
                            newPeriodSliderConfig =
                                calculatePeriodSliderConfig model

                            newPeriodInputText =
                                checkLimit model.periodInputText
                                    newPeriodSliderConfig

                            ( newPeriodSliderModel, _ ) =
                                SliderWithTextfield.update
                                    (SliderWithTextfield.TextfieldMsg
                                        (InternalTextfield.Input (newPeriodInputText |> Maybe.withDefault "0"))
                                    )
                                    model.sliderWithTextfield2
                                    newPeriodSliderConfig
                                    model.periodInputText
                        in
                            { model
                                | radios = radios
                                , sliderWithTextfield2 = newPeriodSliderModel
                                , periodInputText = newPeriodInputText
                            }
                                ! []

        DatePickerMsg msg ->
            let
                ( newDatePicker, datePickerFx, dateEvent ) =
                    DatePicker.update datePickerConfig
                        msg
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

        SliderWithTextfieldMsg1 msg_ ->
            let
                ( newSumSliderModel, newText ) =
                    SliderWithTextfield.update
                        msg_
                        model.sliderWithTextfield1
                        (currentSliderConfig model)
                        model.sumInputText
            in
                { model
                    | sliderWithTextfield1 = newSumSliderModel
                    , sumInputText =
                        newText
                }
                    ! []

        SliderWithTextfieldMsg2 msg_ ->
            let
                ( newModel, newDatePicker, newDate, newText ) =
                    updatePeriodSliderAndDatepicker model msg_
            in
                { model
                    | sliderWithTextfield2 = newModel
                    , datePicker = newDatePicker
                    , date = newDate
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

        ForPayMsg msg_ ->
            let
                ( newTextfieldModel, newText ) =
                    TextfieldNumber.externalUpdate
                        msg_
                        model.forPay
                        forPayConfig
                        model.forPayTextInput
            in
                { model | forPay = newTextfieldModel, forPayTextInput = newText } ! []

        Select n ->
            model ! []

        Open ->
            model ! []


forPayConfig : TextfieldNumber.Config
forPayConfig =
    let
        defaultTextfieldNumber =
            TextfieldNumber.defaultConfig
    in
        { defaultTextfieldNumber
            | extraInside = Just "₽"
            , defaultValue = Just 40000
            , labelText = Just "К выплате"
            , asTitle = True
        }


view : Model -> Html Msg
view model =
    let
        ( rippleOptions, rippleStyles ) =
            Ripple.view False (RippleMsg) model.ripple () ()

        defaultTextfield =
            Textfield.defaultConfig

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
                        model.sliderWithTextfield1
                        (currentSliderConfig model)
                        |> Html.map SliderWithTextfieldMsg1
                    , SliderWithTextfield.view
                        model.periodInputText
                        model.sliderWithTextfield2
                        (calculatePeriodSliderConfig model)
                        |> Html.map SliderWithTextfieldMsg2
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
                        [ TextfieldNumber.view
                            model.forPayTextInput
                            model.forPay
                            forPayConfig
                            |> Html.map ForPayMsg
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
        [ Sub.map SliderWithTextfieldMsg1
            (SliderWithTextfield.subscriptions
                model.sliderWithTextfield1
            )
        , Sub.map SliderWithTextfieldMsg2
            (SliderWithTextfield.subscriptions
                model.sliderWithTextfield2
            )
        ]
