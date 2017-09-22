module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
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
    , datePicker : DatePicker.DatePicker
    , date : Maybe Date
    , radios : Dict String String
    , radioModel1 : RadioButton.Model
    , radioModel2 : RadioButton.Model
    , ripple : Ripple.Model
    }


init : ( Model, Cmd Msg )
init =
    let
        ( dp, datePickerFx ) =
            DatePicker.init
    in
        { textfield = Textfield.defaultModel
        , sliderWithTextfield1 = SliderWithTextfield.defaultModel
        , sliderWithTextfield2 = SliderWithTextfield.defaultModel
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
            in
                { model
                    | radios = Dict.insert group value model.radios
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
                ( new, _ ) =
                    SliderWithTextfield.update
                        msg_
                        model.sliderWithTextfield1
                        swtConf1
            in
                { model | sliderWithTextfield1 = new } ! []

        SliderWithTextfieldMsg2 msg_ ->
            let
                ( new, _ ) =
                    SliderWithTextfield.update
                        msg_
                        model.sliderWithTextfield2
                        swtConf2
            in
                { model | sliderWithTextfield2 = new } ! []

        TextfieldMsg msg_ ->
            let
                ( textfield, effects ) =
                    Textfield.update
                        msg_
                        model.textfield
                        textfieldConfig
            in
                { model | textfield = textfield } ! []

        Select n ->
            model ! []

        Open ->
            model ! []


view : Model -> Html Msg
view model =
    let
        ( rippleOptions, rippleStyles ) =
            Ripple.view False (RippleMsg) model.ripple () ()

        isSelected isDef group name =
            Dict.get group model.radios
                |> Maybe.map ((==) name)
                |> Maybe.withDefault isDef
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
                            [ Options.onClick (OnRadioClick "group-1" "name-1")
                            , RadioButton.selected |> when (isSelected True "group-1" "name-1")
                            , RadioButton.name "name-1"
                            ]
                            [ label [] [ text "Первичный клиент" ] ]
                        ]
                    , div []
                        [ RadioButton.view RadioButtonMsg2
                            model.radioModel2
                            [ Options.onClick (OnRadioClick "group-1" "name-2")
                            , RadioButton.selected |> when (isSelected True "group-1" "name-2")
                            , RadioButton.name "name-1"
                            ]
                            [ label [] [ text "Повторный клиент" ] ]
                        ]
                    ]
                , styled div
                    [ cs "fields" ]
                    [ SliderWithTextfield.view
                        model.sliderWithTextfield1
                        swtConf1
                        |> Html.map SliderWithTextfieldMsg1
                    , SliderWithTextfield.view
                        model.sliderWithTextfield2
                        swtConf2
                        |> Html.map SliderWithTextfieldMsg2
                    , DatePicker.view
                        model.date
                        datePickerConfig
                        model.datePicker
                        |> Html.map DatePickerMsg
                    , Textfield.view
                        model.textfield
                        textfieldConfig
                        |> Html.map TextfieldMsg
                    ]
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
