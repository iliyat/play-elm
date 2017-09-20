module DatepickerDemo exposing (main)

import Date exposing (Date, Day(..), day, dayOfWeek, month, year)
import DatePicker exposing (defaultSettings, DateEvent(..), moreOrLess)
import Html exposing (Html, div, h1, text)
import Html.Attributes


type Msg
    = ToDatePicker DatePicker.Msg


type alias Model =
    { date : Maybe Date
    , datePicker : DatePicker.DatePicker
    }


settings : DatePicker.Settings
settings =
    let
        isDisabled date =
            dayOfWeek date
                |> flip List.member [ Sat, Sun ]
    in
        { defaultSettings
            | isDisabled = always False
        }


init : ( Model, Cmd Msg )
init =
    let
        ( dp, datePickerFx ) =
            DatePicker.init
    in
        { date = Nothing
        , datePicker = dp
        }
            ! [ Cmd.map ToDatePicker datePickerFx ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ date, datePicker } as model) =
    case msg of
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
                h1 [] [ text <| formatDate date ]
        , DatePicker.view date settings datePicker
            |> Html.map ToDatePicker
        ]


formatDate : Date -> String
formatDate d =
    toString (month d) ++ " " ++ toString (day d) ++ ", " ++ toString (year d)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
