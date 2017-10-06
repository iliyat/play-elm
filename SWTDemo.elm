module SWTDemo exposing (main)

import Date exposing (Date, Day(..), Month(..), day, dayOfWeek, month, year)
import Ui.DatePicker as DatePicker
    exposing
        ( defaultSettings
        , DateEvent(..)
        , moreOrLess
        )
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (style)
import Task
import Ui.SliderWithTextfield as SliderWithTextfield
import Date.Extra as Date
import Ui.Slider as Slider
import Ui.Textfield as Textfield


type Msg
    = DatePickerMsg DatePicker.Msg
    | CurrentDate Date
    | SumSliderWithTextfieldMsg SliderWithTextfield.Msg


type alias Model =
    { date : Maybe Date
    , datePicker : DatePicker.DatePicker
    , sumSliderModel : SliderWithTextfield.Model
    , sumInputText : Maybe String
    }


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
                , extra = Just "₽"
                , fullWidth = True
                , labelText = Just "Сумма"
            }
        , extraStatic = Just "₽"
        , extraPlural = Nothing
        }


currentSWTConfig : Model -> SliderWithTextfield.Config
currentSWTConfig model =
    let
        inputOriginalText =
            String.toInt (model.sumInputText |> Maybe.withDefault "0")
                |> Result.withDefault 0

        floatValue =
            toFloat inputOriginalText

        lessThan1000 =
            if inputOriginalText <= 10000 then
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
        SliderWithTextfield.withLimits1 swtConf1 floatValue min max 1


settings : DatePicker.Settings
settings =
    DatePicker.withLabel "Дата погашения"


init : ( Model, Cmd Msg )
init =
    let
        ( dp, _ ) =
            DatePicker.init
    in
        { date = Just <| Date.fromParts 1992 Feb 21 0 0 0 0
        , datePicker = dp
        , sumSliderModel = SliderWithTextfield.defaultModel
        , sumInputText = Nothing
        }
            ! [ Task.perform CurrentDate Date.now ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ date, datePicker } as model) =
    case msg of
        SumSliderWithTextfieldMsg msg_ ->
            let
                ( newSumSliderModel, newText ) =
                    SliderWithTextfield.update
                        msg_
                        model.sumSliderModel
                        (currentSWTConfig model)
                        model.sumInputText

                daysToAdd_ =
                    String.toInt (newText |> Maybe.withDefault "0") |> Result.map (flip (-) 1) |> Result.withDefault 0

                daysToAdd =
                    if daysToAdd_ > 100 then
                        100
                    else
                        daysToAdd_

                dateForDatepicker =
                    Date.add Date.Day daysToAdd <| DatePicker.today model.datePicker
            in
                { model
                    | sumSliderModel = newSumSliderModel
                    , sumInputText = newText
                    , date = Just dateForDatepicker
                }
                    ! []

        CurrentDate today ->
            let
                initDate =
                    (Date.add Date.Day 7 today)
            in
                { model
                    | datePicker = DatePicker.initFromDate initDate today
                    , date = Just initDate
                }
                    ! []

        DatePickerMsg msg ->
            let
                ( newDatePicker, datePickerFx, dateEvent ) =
                    DatePicker.update model.date settings msg datePicker

                newDate =
                    case dateEvent of
                        Changed newDate ->
                            newDate

                        _ ->
                            date

                min =
                    .min <| .sliderConfig <| (currentSWTConfig model)

                max =
                    .max <| .sliderConfig <| (currentSWTConfig model)

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
                    , sumInputText = Just (toString newValue)
                }
                    ! [ Cmd.map DatePickerMsg datePickerFx ]


view : Model -> Html Msg
view ({ date, datePicker } as model) =
    div [ style [ ( "display", "flex" ) ] ]
        [ Html.node "link"
            [ Html.Attributes.rel "stylesheet"
            , Html.Attributes.href "datepicker.css"
            ]
            []
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
        , case date of
            Nothing ->
                h1 [] [ text "Pick a date" ]

            Just date ->
                h1 [] [ text "Selected" ]
        , DatePicker.view date settings datePicker
            |> Html.map DatePickerMsg
        , SliderWithTextfield.view
            model.sumInputText
            model.sumSliderModel
            (currentSWTConfig model)
            |> Html.map SumSliderWithTextfieldMsg
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map SumSliderWithTextfieldMsg
            (SliderWithTextfield.subscriptions
                model.sumSliderModel
            )
        ]
