module FormDemoWithTextfield exposing (main)

import Html exposing (Html, div, h1, text, p)
import Html.Attributes as Attrs
import Ui.Button as Button
import Ui.Textfield as Textfield


type Msg
    = ButtonMsg Button.Msg
    | TextfieldMsg Textfield.Msg


type alias Model =
    { buttonModel : Button.Model
    , textfield : Textfield.Model
    , textfieldInput : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    { buttonModel = Button.defaultModel
    , textfield = Textfield.defaultModel
    , textfieldInput = Nothing
    }
        ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextfieldMsg msg_ ->
            let
                ( newTextfieldModel, newText ) =
                    Textfield.externalUpdate
                        msg_
                        model.textfield
                        Textfield.defaultConfig
                        model.textfieldInput
            in
                ( { model
                    | textfield = newTextfieldModel
                    , textfieldInput =
                        newText
                  }
                , Cmd.none
                )

        ButtonMsg msg_ ->
            let
                ( new, effects ) =
                    Button.update msg_ model.buttonModel
            in
                ( { model | buttonModel = new }, effects |> Cmd.map ButtonMsg )


view : Model -> Html Msg
view model =
    let
        tfConfig =
            Textfield.defaultConfig

        passportSeriesConfig =
            { tfConfig | labelText = Just "Серия" }
    in
        div []
            [ Button.view ButtonMsg model.buttonModel [] [ text "Test" ]
            , Textfield.view
                model.textfieldInput
                model.textfield
                passportSeriesConfig
                |> Html.map TextfieldMsg
            , Html.node "link"
                [ Attrs.rel "stylesheet"
                , Attrs.href "material-components-web.css"
                ]
                []
            , Html.node "link"
                [ Attrs.rel "stylesheet"
                , Attrs.href "main.css"
                ]
                []
            , Html.node "link"
                [ Attrs.rel "stylesheet"
                , Attrs.href "https://fonts.googleapis.com/icon?family=Material+Icons"
                ]
                []
            , Html.node "link"
                [ Attrs.rel "stylesheet", Attrs.href "https://fonts.googleapis.com/css?family=Roboto:300,400,500" ]
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
