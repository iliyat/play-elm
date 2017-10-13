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


type alias Output =
    { passportSeries : String
    , passportNumber : String
    , dateOfBirth : String
    , issuedAt : String
    , lastName : String
    , firstName : String
    , middleName : String
    }


type alias Model =
    { buttonModel : Button.Model
    , formBinder : FormBinder.Model () Output
    }


validation : Validation () Output
validation =
    map7 Output
        (field "passportSeries" (string |> andThen (minLength 4)))
        (field "passportNumber" string)
        (field "issuedAt" (string))
        (field "dateOfBirth" string)
        (field "lastName" string)
        (field "firstName" string)
        (field "middleName" string)


init : ( Model, Cmd Msg )
init =
    let
        initialUi =
            [ ( "passportSeries", FormBinder.TextfieldModel Textfield.defaultModel )
            , ( "passportNumber", FormBinder.TextfieldModel Textfield.defaultModel )
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


formView : Model -> Form () Output -> Html Msg
formView ({ formBinder } as model) form =
    let
        tfConf =
            Textfield.defaultConfig

        dpConfg =
            DatePicker.defaultSettings

        passportNumber =
            { tfConf
                | labelText = Just "Номер"
                , mask = Just "##"
                , formName = Just "passportNumber"
            }

        passportSeries =
            { tfConf
                | labelText = Just "Серия"
                , mask = Just "####"
                , formName = Just "passportSeries"
            }

        issuedAt =
            { tfConf
                | labelText = Just "Дата выдачи"
                , formName = Just "issuedAt"
            }

        dateOfBirth =
            { tfConf
                | labelText = Just "Дата рождения"
                , formName = Just "dateOfBirth"
                , mask = Just "##.##.####"
                , numbered = True
            }

        lastName =
            { tfConf
                | labelText = Just "Фамилия"
                , formName = Just "lastName"
            }

        firstName =
            { tfConf
                | labelText = Just "Имя"
                , formName = Just "firstName"
            }

        middleName =
            { tfConf
                | labelText = Just "Отчество"
                , formName = Just "middleName"
            }
    in
        div []
            [ div [ style [ ( "display", "flex" ) ] ]
                [ FormBinder.textfield FormBinderMsg formBinder passportSeries
                , FormBinder.textfield FormBinderMsg formBinder passportNumber
                , FormBinder.datePicker FormBinderMsg formBinder (DatePicker.withTextfield issuedAt)
                , FormBinder.datePicker FormBinderMsg formBinder (DatePicker.withTextfield dateOfBirth)
                ]
            , div [ style [ ( "display", "flex" ) ] ]
                [ FormBinder.textfield FormBinderMsg formBinder lastName
                , FormBinder.textfield FormBinderMsg formBinder firstName
                , FormBinder.textfield FormBinderMsg formBinder middleName
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
    let
        form =
            model.formBinder.form
    in
        div []
            [ (formView model form)
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
