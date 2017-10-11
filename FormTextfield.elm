module FormTextfield exposing (render, update, Msg, Model)

import Html exposing (Html, div, h1, text, p, input)
import Html.Attributes as Attr
import Html.Events as Events
import Ui.Textfield as Textfield
import Ui.Button as Button
import Form
import Form.Field as Field exposing (Field, FieldValue(..))
import Form.Validate as Validate exposing (..)


type Msg
    = ButtonMsg Button.Msg
    | TextfieldMsg Textfield.Msg
    | FormMsg Form.Msg


type alias Model =
    { textfieldModel : Textfield.Model
    , fieldModel : Input String String
    }


init : ( Model, Cmd Msg )
init =
    { textfieldModel = Textfield.defaultModel
    , fieldModel = def
    }
        ! []



-- ( { model | form = Form.update validation formMsg form }, Cmd.none )


update : Msg -> Model -> Validation e a -> ( Model, Cmd Msg )
update msg model validation =
    case msg of
        FormMsg formMsg ->
            model ! []

        TextfieldMsg msg_ ->
            let
                ( newTextfieldModel, newText ) =
                    Textfield.externalUpdate
                        msg_
                        model.textfieldModel
                        Textfield.defaultConfig
                        Nothing
            in
                ( { model | textfieldModel = newTextfieldModel }, Cmd.none )

        ButtonMsg msg_ ->
            let
                ( new, effects ) =
                    Button.update msg_ model.buttonModel
            in
                ( { model | buttonModel = new }, effects |> Cmd.map ButtonMsg )


view : Model -> Html Msg
view model =
    div []
        [ Button.view ButtonMsg model.buttonModel [] [ text "Test" ]
        ]



-- type alias Input e a =
--     Form.FieldState e a -> List (Html.Attribute Form.Msg) -> Html Form.Msg


render : Form.FieldState String String -> Model -> Html Form.Msg
render state model =
    let
        attrs =
            []
    in
        input
            [ Events.onFocus (Form.Focus state.path)
            , Events.onFocus
            ]
            []



-- Textfield.view state.value model.textfieldModel Textfield.defaultConfig |> Html.map TextfieldMsg
