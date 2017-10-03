module DatepickerDemo exposing (main)

import Date exposing (Date, Day(..), Month(..), day, dayOfWeek, month, year)
import Ui.DatePicker as DatePicker
    exposing
        ( defaultSettings
        , DateEvent(..)
        , moreOrLess
        )
import Html exposing (Html, div, h1, text)
import Html.Attributes
import Task
import Date.Extra as Date


type Msg
    = ToDatePicker DatePicker.Msg
    | CurrentDate Date


type alias Model =
    { date : Maybe Date
    , datePicker : DatePicker.DatePicker
    }


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
        }
            ! [ Task.perform CurrentDate Date.now ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ date, datePicker } as model) =
    case msg of
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

        ToDatePicker msg ->
            let
                ( newDatePicker, datePickerFx, dateEvent ) =
                    DatePicker.update settings msg datePicker

                newDate =
                    case dateEvent of
                        Changed newDate ->
                            newDate

                        _ ->
                            date
            in
                { model
                    | date = newDate
                    , datePicker = newDatePicker
                }
                    ! [ Cmd.map ToDatePicker datePickerFx ]


view : Model -> Html Msg
view ({ date, datePicker } as model) =
    div []
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
            |> Html.map ToDatePicker
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
