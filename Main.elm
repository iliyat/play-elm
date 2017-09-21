module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Slider
import Textfield
import SliderWithTextfield
import Utils exposing (..)
import Date exposing (Date)
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
        }
            ! [ Cmd.map DatePickerMsg datePickerFx ]


type Msg
    = Open
    | SliderWithTextfieldMsg1 SliderWithTextfield.Msg
    | SliderWithTextfieldMsg2 SliderWithTextfield.Msg
    | TextfieldMsg Textfield.Msg
    | DatePickerMsg DatePicker.Msg
    | Select Int


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
    DatePicker.defaultSettings


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
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
                ( new, effects ) =
                    SliderWithTextfield.update SliderWithTextfieldMsg1
                        msg_
                        model.sliderWithTextfield1
                        swtConf1
            in
                ( { model | sliderWithTextfield1 = new }, effects )

        SliderWithTextfieldMsg2 msg_ ->
            let
                ( new, effects ) =
                    SliderWithTextfield.update SliderWithTextfieldMsg2
                        msg_
                        model.sliderWithTextfield2
                        swtConf2
            in
                ( { model | sliderWithTextfield2 = new }, effects )

        TextfieldMsg msg_ ->
            let
                ( textfield, effects ) =
                    Textfield.update TextfieldMsg
                        msg_
                        model.textfield
                        textfieldConfig
            in
                ( { model | textfield = textfield }, effects )

        Select n ->
            ( model, Cmd.none )

        Open ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div []
        [ div
            [ style
                [ ( "margin", "54px" )
                , ( "display", "flex" )
                , ( "justify-content", "space-between" )
                ]
            ]
            [ SliderWithTextfield.view SliderWithTextfieldMsg1
                model.sliderWithTextfield1
                swtConf1
            , SliderWithTextfield.view SliderWithTextfieldMsg2
                model.sliderWithTextfield2
                swtConf2
            , DatePicker.view
                model.date
                datePickerConfig
                model.datePicker
                |> Html.map DatePickerMsg
            , Textfield.view model.textfield textfieldConfig |> Html.map TextfieldMsg
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
