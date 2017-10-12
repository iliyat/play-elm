module DemoTemplate exposing (main)

import Html exposing (Html, div, h1, text, p)
import Html.Attributes as Attrs
import Ui.Button as Button


type Msg
    = ButtonMsg Button.Msg


type alias Model =
    { buttonModel : Button.Model
    }


init : ( Model, Cmd Msg )
init =
    { buttonModel = Button.defaultModel
    }
        ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
