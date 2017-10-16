module FormDemo exposing (main)

import Html exposing (Html, div, h1, text, p, label, button)
import Html.Attributes as Attr exposing (class, style)
import Form exposing (Form)
import Form.Validate as Validate exposing (..)
import Ui.Options as Options exposing (when)
import Ui.DatePicker as DatePicker
    exposing
        ( defaultSettings
        , DateEvent(..)
        , moreOrLess
        )
import Ui.Button as Button
import Ui.Textfield as Textfield
import FormBinder
import Regex exposing (Regex)


type alias Output =
    { passportSeries : String
    , passportNumber : String
    , dateOfBirth : String
    , issuedAt : String
    , lastName : String
    , firstName : String
    , middleName : String
    , code : String
    , issuedBy : String
    , placeOfBirth : String
    }


type alias Model =
    { buttonModel : Button.Model
    , formBinder : FormBinder.Model () Output
    }


type LocalError
    = InvalidCode


max255 : Validation e String
max255 =
    string |> andThen (maxLength 255)


validation : Validation () Output
validation =
    map8 Output
        (field "passportSeries"
            (string
                |> andThen (minLength 4)
                |> andThen (maxLength 4)
            )
        )
        (field "passportNumber"
            (string
                |> andThen (minLength 6)
                |> andThen
                    (maxLength 6)
            )
        )
        (field "issuedAt" string)
        (field "dateOfBirth" string)
        (field "lastName" max255)
        (field "firstName" max255)
        (field "middleName" max255)
        (field "code" string)
        |> andMap (field "issuedBy" max255)
        |> andMap (field "placeOfBirth" max255)


init : ( Model, Cmd Msg )
init =
    let
        dpModel =
            DatePicker.DatePicker <| DatePicker.defaultModel

        initialUi =
            [ ( "issuedAt", FormBinder.DatePickerModel dpModel )
            , ( "dateOfBirth", FormBinder.DatePickerModel dpModel )
            ]

        ( formBinderModel, formBinderFx ) =
            FormBinder.initial [] validation initialUi
    in
        { buttonModel = Button.defaultModel
        , formBinder = formBinderModel
        }
            ! [ Cmd.map FormBinderMsg formBinderFx ]


type Msg
    = ButtonMsg Button.Msg
    | FormBinderMsg FormBinder.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ formBinder } as model) =
    let
        form =
            formBinder.form
    in
        case msg of
            FormBinderMsg msg ->
                let
                    ( newModel, effects ) =
                        FormBinder.update msg model.formBinder validation
                in
                    ( { model | formBinder = newModel }, Cmd.map FormBinderMsg effects )

            ButtonMsg msg_ ->
                let
                    ( new, effects ) =
                        Button.update msg_ model.buttonModel
                in
                    ( { model | buttonModel = new }, effects |> Cmd.map ButtonMsg )


formView : Model -> Html Msg
formView ({ formBinder, buttonModel } as model) =
    let
        form =
            formBinder.form

        tfConf =
            Textfield.defaultConfig

        dpConfg =
            DatePicker.defaultSettings

        passportSeries =
            { tfConf
                | labelText = Just "Серия"
                , mask = Just "####"
                , formName = Just "passportSeries"
                , numbered = True
                , tabindex = 1
                , width = 64
            }

        passportNumber =
            { tfConf
                | labelText = Just "Номер"
                , mask = Just "######"
                , formName = Just "passportNumber"
                , tabindex = 2
                , width = 72
            }

        issuedAt =
            { tfConf
                | labelText = Just "Дата выдачи"
                , formName = Just "issuedAt"
                , tabindex = 3
                , width = 150
            }

        dateOfBirth =
            { tfConf
                | labelText = Just "Дата рождения"
                , formName = Just "dateOfBirth"
                , tabindex = 4
                , width = 150
            }

        lastName =
            { tfConf
                | labelText = Just "Фамилия"
                , formName = Just "lastName"
                , tabindex = 5
                , width = 146
            }

        firstName =
            { tfConf
                | labelText = Just "Имя"
                , formName = Just "firstName"
                , tabindex = 6
                , width = 144
            }

        middleName =
            { tfConf
                | labelText = Just "Отчество"
                , formName = Just "middleName"
                , tabindex = 7
                , width = 200
            }

        code =
            { tfConf
                | labelText = Just "Код подразделения"
                , formName = Just "code"
                , mask = Just "###-###"
                , tabindex = 8
                , width = 150
            }

        issuedBy =
            { tfConf
                | labelText = Just "Кем выдан"
                , formName = Just "issuedBy"
                , tabindex = 9
                , width = 336
            }

        placeOfBirth =
            { tfConf
                | labelText = Just "Место рождения"
                , formName = Just "placeOfBirth"
                , tabindex = 10
                , width = 168
            }
    in
        div []
            [ div [ class "ui-form-row" ]
                [ FormBinder.textfield FormBinderMsg formBinder passportSeries
                , FormBinder.textfield FormBinderMsg formBinder passportNumber
                , FormBinder.datePicker FormBinderMsg formBinder (DatePicker.withTextfield issuedAt)
                , FormBinder.datePicker FormBinderMsg formBinder (DatePicker.withTextfield dateOfBirth)
                ]
            , div [ class "ui-form-row" ]
                [ FormBinder.textfield FormBinderMsg formBinder lastName
                , FormBinder.textfield FormBinderMsg formBinder firstName
                , FormBinder.textfield FormBinderMsg formBinder middleName
                ]
            , div [ class "ui-form-row" ]
                [ FormBinder.textfield FormBinderMsg formBinder code
                , FormBinder.textfield FormBinderMsg formBinder issuedBy
                , FormBinder.textfield FormBinderMsg formBinder placeOfBirth
                ]
            , Button.view ButtonMsg
                model.buttonModel
                [ Button.ripple
                , Button.raised
                , Button.primary
                , Options.onClick (FormBinderMsg <| FormBinder.FormMsg Form.Submit)
                ]
                [ text "Submit" ]
            ]


view : Model -> Html Msg
view model =
    div []
        [ (formView model)
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
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map FormBinderMsg
            (FormBinder.subscriptions
                model.formBinder
            )
        ]
