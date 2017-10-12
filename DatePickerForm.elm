module DatePickerForm exposing (view, update)

import Date exposing (Date, Day(..), Month(..), day, dayOfWeek, month, year)
import Ui.DatePicker as DatePicker
    exposing
        ( defaultSettings
        , DateEvent(..)
        , moreOrLess
        )
import Html exposing (Html, div, h1, text)
import Task
import Date.Extra as Date
import Form exposing (Form)
import Form.Input as Form
import Form.Field as Field
import Form.Validate as Validate exposing (..)
import Ui.DatePickerDate exposing (formatDate)


type Msg
    = DatePickerMsg DatePicker.Msg
    | CurrentDate Date
    | FormMsg Form.Msg


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


update : Form () a -> Validation () a -> Msg -> Model -> ( Model, Cmd Msg, Form () a )
update form validation msg ({ date, datePicker } as model) =
    case msg of
        FormMsg formMsg ->
            ( model, Cmd.none, form )

        CurrentDate today ->
            let
                initDate =
                    (Date.add Date.Day 7 today)
            in
                ( { model
                    | datePicker = DatePicker.initFromDate initDate today
                    , date = Just initDate
                  }
                , Cmd.none
                , form
                )

        DatePickerMsg msg_ ->
            let
                ( newDatePicker, datePickerFx, dateEvent ) =
                    DatePicker.update
                        model.date
                        DatePicker.defaultSettings
                        msg_
                        model.datePicker

                ( newDate, newFormModel ) =
                    case dateEvent of
                        Changed newDate ->
                            case newDate of
                                Just d ->
                                    let
                                        _ =
                                            Debug.log "Changed date " (formatDate d)

                                        fv =
                                            Field.String (formatDate d)

                                        newFormModel_ =
                                            Form.update validation (Form.Input "name" Form.Text (fv)) form
                                    in
                                        ( newDate, newFormModel_ )

                                _ ->
                                    ( newDate, form )

                        _ ->
                            ( model.date, form )
            in
                ( { model | date = newDate, datePicker = newDatePicker }
                , Cmd.map DatePickerMsg datePickerFx
                , newFormModel
                )


view : Model -> Html Msg
view ({ date, datePicker } as model) =
    DatePicker.view
        model.date
        settings
        model.datePicker
        |> Html.map DatePickerMsg
