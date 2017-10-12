module FormDemo exposing (main)

import Html exposing (Html, div, h1, text, p, label, button)
import Html.Attributes as Attr exposing (class, style)
import Html.Events as Events exposing (..)
import Form exposing (Form)
import Form.Validate as Validate exposing (..)
import Form.Input as Form
import Form.Field as Field
import Ui.Options as Options exposing (when)
import Ui.DatePicker as DatePicker
    exposing
        ( defaultSettings
        , DateEvent(..)
        , moreOrLess
        )
import Ui.Button as Button
import Task
import Ui.Textfield as Textfield
import Date exposing (Date, Day(..), Month(..), day, dayOfWeek, month, year)
import Date.Extra as Date
import Ui.DatePickerDate exposing (formatDate)


type alias Output =
    { passportSeries : String
    , passportNumber : String
    , issuedAt : String
    }


type alias Model =
    { buttonModel : Button.Model
    , form : Form () Output
    , date : Maybe Date
    , datePicker : DatePicker.DatePicker
    , textfield : Textfield.Model
    }


validation : Validation () Output
validation =
    map3 Output
        (field "passportSeries" (string |> andThen (minLength 4)))
        (field "passportNumber" string)
        (field "issuedAt" (string |> andThen (minLength 30)))


init : ( Model, Cmd Msg )
init =
    let
        ( dp, _ ) =
            DatePicker.init
    in
        { date = Just <| Date.fromParts 1992 Feb 21 0 0 0 0
        , datePicker = dp
        , buttonModel = Button.defaultModel
        , form = Form.initial [] validation
        , textfield = Textfield.defaultModel
        }
            ! [ Task.perform CurrentDate Date.now ]


type Msg
    = ButtonMsg Button.Msg
    | FormMsg Form.Msg
    | DatePickerMsg DatePicker.Msg
    | CurrentDate Date
    | TextfieldMsg String Textfield.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ form } as model) =
    case msg of
        TextfieldMsg fieldName msg_ ->
            let
                field =
                    Form.getFieldAsString fieldName form

                ( newTextfieldModel, newText ) =
                    Textfield.externalUpdate
                        msg_
                        model.textfield
                        Textfield.defaultConfig
                        field.value

                fieldValue =
                    Field.String (newText |> Maybe.withDefault "")

                newFormModel_ =
                    Form.update validation
                        (Form.Input fieldName
                            Form.Text
                            fieldValue
                        )
                        form
            in
                ( { model
                    | textfield = newTextfieldModel
                    , form = newFormModel_
                  }
                , Cmd.none
                )

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

        FormMsg formMsg ->
            ( { model | form = Form.update validation formMsg form }, Cmd.none )

        ButtonMsg msg_ ->
            let
                ( new, effects ) =
                    Button.update msg_ model.buttonModel
            in
                ( { model | buttonModel = new }, effects |> Cmd.map ButtonMsg )

        DatePickerMsg msg ->
            let
                ( newDatePicker, datePickerFx, dateEvent ) =
                    DatePicker.update
                        model.date
                        DatePicker.defaultSettings
                        msg
                        model.datePicker

                ( newDate, newFormModel ) =
                    case dateEvent of
                        Changed newDate ->
                            case newDate of
                                Just d ->
                                    let
                                        fv =
                                            Field.String (formatDate d)

                                        newFormModel_ =
                                            Form.update validation
                                                (Form.Input
                                                    "issuedAt"
                                                    Form.Text
                                                    (fv)
                                                )
                                                form
                                    in
                                        ( newDate, newFormModel_ )

                                _ ->
                                    ( newDate, form )

                        _ ->
                            ( model.date, form )
            in
                { model
                    | date = newDate
                    , datePicker = newDatePicker
                    , form = newFormModel
                }
                    ! [ Cmd.map DatePickerMsg datePickerFx ]


errorText : Form.FieldState e String -> Html Never
errorText field =
    let
        classList_ =
            [ ( "mdc-textfield-helptext mdc-textfield-helptext--validation-msg", True )
            , ( "mdc-textfield-helptext--persistent", True )
            ]
    in
        case field.liveError of
            Just error ->
                p [ Attr.classList classList_ ] [ text (toString error) ]

            Nothing ->
                text ""


textfieldView : Form.FieldState e String -> String -> Bool -> Html Form.Msg
textfieldView state labelText required =
    let
        isInvalid field =
            case field.liveError of
                Just error ->
                    True

                Nothing ->
                    False

        classList_ state =
            [ ( "mdc-textfield mdc-textfield--upgraded", True )
            , ( "mdc-textfield--focused", state.hasFocus )
            , ( "mdc-textfield--fullwidth", False )
            , ( "mdc-textfield--invalid", isInvalid state )
            ]

        inputAttrs =
            [ Attr.class "mdc-textfield__input" ]

        simpleStyle =
            { labelBottom = "8px"
            , labelFontSize = "16px"
            , height = "48px"
            , fontSize = "18px"
            }

        labelText_ =
            labelText
                ++ (if required then
                        " *"
                    else
                        ""
                   )
    in
        div []
            [ div [ Attr.classList <| classList_ state ]
                [ Form.textInput state inputAttrs
                , label
                    [ Attr.classList
                        [ ( "mdc-textfield__label mdc-typography", True )
                        , ( "mdc-textfield__label--float-above"
                          , state.hasFocus || state.isChanged
                          )
                        ]
                    , Attr.style
                        [ ( "bottom", simpleStyle.labelBottom )
                        , ( "font-size", simpleStyle.labelFontSize )
                        ]
                    ]
                    [ text labelText_ ]
                ]
            , errorText state |> Html.map never
            ]


formView : Model -> Form () Output -> Html Msg
formView model form =
    let
        tfConf =
            Textfield.defaultConfig

        passportSeries =
            Form.getFieldAsString "passportSeries" form

        passportNumber =
            Form.getFieldAsString "passportNumber" form

        issuedAt =
            Form.getFieldAsString "issuedAt" form

        hasError field =
            if field.liveError /= Nothing then
                True
            else
                False

        conf label formField =
            { tfConf
                | labelText = Just "Серия"
                , mask = Just "####"
                , invalid = hasError formField
                , required = True
                , errorText =
                    formField.liveError
                        |> Maybe.map toString
                        |> Maybe.withDefault ""
            }
    in
        div []
            [ div [ style [ ( "display", "flex" ) ] ]
                [ textfieldView passportNumber "Номер" True |> Html.map FormMsg
                , Textfield.view
                    passportSeries.value
                    model.textfield
                    (conf "Серия" passportSeries)
                    |> Html.map (TextfieldMsg "passportSeries")
                , DatePicker.view
                    model.date
                    (DatePicker.withLabel "Дата погашения" (hasError issuedAt))
                    model.datePicker
                    |> Html.map DatePickerMsg
                ]
            , button
                [ onClick (FormMsg Form.Submit) ]
                [ text "Submit" ]
            ]


view : Model -> Html Msg
view ({ form } as model) =
    let
        issuedAt =
            Form.getFieldAsString "issuedAt" form
    in
        div []
            [ Button.view ButtonMsg model.buttonModel [] [ text "Test" ]
            , (formView model form)
            , Html.node "link"
                [ Attr.rel "stylesheet"
                , Attr.href "material-components-web.css"
                ]
                []
            , Html.node "link"
                [ Attr.rel "stylesheet"
                , Attr.href "main.css"
                ]
                []
            , Html.node "link"
                [ Attr.rel "stylesheet"
                , Attr.href "datePicker.css"
                ]
                []
            , Html.node "link"
                [ Attr.rel "stylesheet"
                , Attr.href "https://fonts.googleapis.com/icon?family=Material+Icons"
                ]
                []
            , Html.node "link"
                [ Attr.rel "stylesheet", Attr.href "https://fonts.googleapis.com/css?family=Roboto:300,400,500" ]
                []
            ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
