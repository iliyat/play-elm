module FormDemo exposing (main)

import Html exposing (Html, div, h1, text, p, label, button)
import Html.Attributes as Attr exposing (class, style)
import Html.Events as Events exposing (..)
import Form exposing (Form)
import Form.Validate as Validate exposing (..)
import Form.Input as Form
import Ui.Datepicker as DatePicker
import Ui.Button as Button


type alias Output =
    { passportSeries : String
    , passportNumber : String
    , issuedAt : String
    }


type alias Model =
    { buttonModel : Button.Model
    , form : Form () Output
    }


validation : Validation () Output
validation =
    map3 Output
        (field "passportSeries" (string |> andThen (minLength 4)))
        (field "passportNumber" string)
        (field "issuedAt" string)


init : ( Model, Cmd Msg )
init =
    { buttonModel = Button.defaultModel
    , form = Form.initial [] validation
    }
        ! []


type Msg
    = ButtonMsg Button.Msg
    | FormMsg Form.Msg
    | DatepickerMsg DatePicker.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ form } as model) =
    case msg of
        FormMsg formMsg ->
            ( { model | form = Form.update validation formMsg form }, Cmd.none )

        ButtonMsg msg_ ->
            let
                ( new, effects ) =
                    Button.update msg_ model.buttonModel
            in
                ( { model | buttonModel = new }, effects |> Cmd.map ButtonMsg )


textfieldView : Form.FieldState e String -> String -> Bool -> Html Form.Msg
textfieldView state labelText required =
    let
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
            , errorText state
            ]


formView : Form () Output -> Html Form.Msg
formView form =
    let
        passportSeries =
            Form.getFieldAsString "passportSeries" form

        passportNumber =
            Form.getFieldAsString "passportNumber" form

        issuedAt =
            Form.getFieldAsString "issuedAt" form
    in
        div []
            [ div [ style [ ( "display", "flex" ) ] ]
                [ textfieldView passportSeries "Серия" True
                , textfieldView passportNumber "Номер" True
                , textfieldView issuedAt "Дата выдачи" True
                ]
            , button
                [ onClick Form.Submit ]
                [ text "Submit" ]
            ]


view : Model -> Html Msg
view ({ form } as model) =
    div []
        [ Button.view ButtonMsg model.buttonModel [] [ text "Test" ]
        , (formView form) |> Html.map FormMsg
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
