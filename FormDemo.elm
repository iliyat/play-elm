module FormDemo exposing (main)

import Html exposing (Html, div, h1, text, p, label, button)
import Html.Attributes as Attr exposing (class)
import Html.Events exposing (..)
import Form exposing (Form)
import Form.Validate as Validate exposing (..)
import Form.Input as Input
import Ui.Button as Button
import Ui.Textfield as Textfield


type alias Output =
    { name : String
    }


type alias Model =
    { buttonModel : Button.Model
    , form : Form () Output
    }


validation : Validation () Output
validation =
    map Output
        (field "name" email)


init : ( Model, Cmd Msg )
init =
    { buttonModel = Button.defaultModel
    , form = Form.initial [] validation
    }
        ! []


type Msg
    = ButtonMsg Button.Msg
    | FormMsg Form.Msg



-- | TextfieldMsg Textfield.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ form } as model) =
    case msg of
        FormMsg formMsg ->
            ( { model | form = Form.update validation formMsg form }, Cmd.none )

        -- TextfieldMsg msg_ ->
        --     let
        --         ( newTextfieldModel, newText ) =
        --             Textfield.externalUpdate
        --                 msg_
        --                 model.textfield
        --                 Textfield.defaultConfig
        --                 Nothing
        --     in
        --         ( { model | textfield = newTextfieldModel }, Cmd.none )
        --
        ButtonMsg msg_ ->
            let
                ( new, effects ) =
                    Button.update msg_ model.buttonModel
            in
                ( { model | buttonModel = new }, effects |> Cmd.map ButtonMsg )


formView : Form () Output -> Html Form.Msg
formView form =
    let
        tfConfig =
            Textfield.defaultConfig

        dateOfBirthConfig =
            { tfConfig | labelText = Just "Дата рождения" }

        errorFor field =
            case field.liveError of
                Just error ->
                    div [ class "error" ] [ text (toString error) ]

                Nothing ->
                    text ""

        name =
            Form.getFieldAsString "name" form

        _ =
            Debug.log "name" name
    in
        div []
            [ label [] [ text "Name" ]
            , Input.textInput name []
            , errorFor name
            , button
                [ onClick Form.Submit ]
                [ text "Submit" ]

            -- , Textfield.view (Just "122312")
            --     Textfield.defaultModel
            --     dateOfBirthConfig
            --     |> Html.map TextfieldMsg
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
