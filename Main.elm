module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events
import Slider
import Textfield
import SliderWithTextfield
import RadioButton
import Ripple
import Dict exposing (Dict)
import Utils exposing (..)
import Date exposing (Date)
import Elevation
import Typography
import Internal.Textfield
import Options exposing (styled, cs, css, when)
import DatePicker
    exposing
        ( defaultSettings
        , DateEvent(..)
        , moreOrLess
        )


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
    }


init : ( Model, Cmd Msg )
init =
    let
        ( dp, datePickerFx ) =
            DatePicker.init
    in
        { textfield = Textfield.defaultModel
        , textInput = Nothing
        , sliderWithTextfield1 = SliderWithTextfield.defaultModel
        , sliderWithTextfield2 = SliderWithTextfield.defaultModel
        , sumInputText = Just "2000"
        , periodInputText = Just "2000"
        , datePicker = dp
        , date = Nothing
        , radios = Dict.fromList []
        , radioModel1 = RadioButton.defaultModel
        , radioModel2 = RadioButton.defaultModel
        , ripple = Ripple.defaultModel
        }
            ! [ Cmd.map DatePickerMsg datePickerFx ]


type Msg
    = Open
    | SliderWithTextfieldMsg1 SliderWithTextfield.Msg
    | SliderWithTextfieldMsg2 SliderWithTextfield.Msg
    | TextfieldMsg Textfield.Msg
    | DatePickerMsg DatePicker.Msg
    | Select Int
    | OnRadioClick String String
    | RadioButtonMsg1 RadioButton.Msg
    | RadioButtonMsg2 RadioButton.Msg
    | RippleMsg Ripple.Msg


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
            | defaultValue = Just "Промокод"
            , readonly = False
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


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
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
                            newValue =
                                checkLimit model.sumInputText sumSliderPrimaryConfig

                            ( newSwtModel, newStr ) =
                                SliderWithTextfield.update
                                    (SliderWithTextfield.TextfieldMsg (Internal.Textfield.Input (newValue |> Maybe.withDefault "0")))
                                    model.sliderWithTextfield1
                                    sumSliderPrimaryConfig
                                    model.sumInputText
                        in
                            { model
                                | radios = radios
                                , sliderWithTextfield1 = newSwtModel
                                , sumInputText = newStr
                            }
                                ! []

                    _ ->
                        let
                            newValue =
                                checkLimit model.sumInputText sumSliderSecondaryConfig

                            ( newSwtModel, newStr ) =
                                SliderWithTextfield.update
                                    (SliderWithTextfield.TextfieldMsg (Internal.Textfield.Input (newValue |> Maybe.withDefault "0")))
                                    model.sliderWithTextfield1
                                    sumSliderSecondaryConfig
                                    model.sumInputText
                        in
                            { model
                                | radios = radios
                                , sliderWithTextfield1 = newSwtModel
                                , sumInputText = newStr
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
                ( newModel, newText ) =
                    SliderWithTextfield.update
                        msg_
                        model.sliderWithTextfield1
                        (currentSliderConfig model)
                        model.sumInputText
            in
                { model
                    | sliderWithTextfield1 = newModel
                    , sumInputText =
                        newText
                }
                    ! []

        SliderWithTextfieldMsg2 msg_ ->
            let
                ( newModel, newText ) =
                    SliderWithTextfield.update
                        msg_
                        model.sliderWithTextfield2
                        swtConf2
                        model.periodInputText
            in
                { model
                    | sliderWithTextfield2 = newModel
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
                        swtConf2
                        |> Html.map SliderWithTextfieldMsg2
                    , DatePicker.view
                        model.date
                        datePickerConfig
                        model.datePicker
                        |> Html.map DatePickerMsg
                    , Textfield.view model.textInput
                        model.textfield
                        textfieldConfig
                        |> Html.map TextfieldMsg
                    ]
                ]
            , styled Html.div
                [ cs "main-container"
                , Elevation.z1
                ]
                [ styled div [ Typography.headline ] [ text "Расчет займа" ]
                ]
            , Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "mdc.css" ] []
            , Html.node "script" [ Html.Attributes.src "mdc.js" ] []
            , Html.node "link"
                [ Html.Attributes.rel "stylesheet"
                , Html.Attributes.href "https://unpkg.com/material-components-web@latest/dist/material-components-web.min.css"
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
