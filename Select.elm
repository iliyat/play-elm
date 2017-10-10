module Select exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style, class)
import Ui.Textfield as Textfield
import Icons.Icon as Icon
import Html.Events as Events
import Menu


type alias Model =
    { textfield : Textfield.Model
    , menu : Menu.Model
    , selected : Maybe String
    }


defaultModel : Model
defaultModel =
    { textfield = Textfield.defaultModel
    , menu = Menu.defaultModel
    , selected = Nothing
    }


type Msg
    = TextfieldMsg Textfield.Msg
    | MenuMsg Menu.Msg


update : Msg -> Model -> ( Model, Cmd m )
update msg model =
    case msg of
        TextfieldMsg msg_ ->
            let
                ( newTextfieldModel, newText ) =
                    Textfield.externalUpdate
                        msg_
                        model.textfield
                        Textfield.defaultConfig
                        model.selected
            in
                ( { model | textfield = newTextfieldModel }, Cmd.none )

        MenuMsg msg_ ->
            let
                ( m, _ ) =
                    Menu.update MenuMsg msg_ model.menu
            in
                ( { model | menu = m }, Cmd.none )


type alias Config =
    { items : List String
    , textfieldConfig : Textfield.Config
    , selected : Maybe String
    }


defaultConfig : Config
defaultConfig =
    { items = [], textfieldConfig = Textfield.defaultConfig, selected = Nothing }


config : String -> Maybe String -> List String -> Config
config label selected items =
    let
        tfConfig =
            Textfield.defaultConfig

        setLabel =
            { tfConfig | labelText = Just label }
    in
        { textfieldConfig = setLabel
        , items = items
        , selected = selected
        }


withLabel : String -> Config
withLabel label =
    let
        tfConfig =
            Textfield.defaultConfig

        setLabel =
            { tfConfig | labelText = Just label }
    in
        { textfieldConfig = setLabel
        , items = []
        , selected = Nothing
        }


view : Model -> Config -> Html Msg
view model config =
    let
        items =
            List.map (\e -> li [ class "mdc-list-item" ] [ text e ]) config.items
    in
        div
            [ Menu.attach MenuMsg
            , style
                [ ( "position", "relative" )
                , ( "width", "198px" )
                , ( "height", "48px" )
                , ( "max-height", "48px" )
                , ( "display", "inline-flex" )
                , ( "align-items", "center" )
                , ( "cursor", "pointer" )
                ]
            ]
            [ Textfield.viewReadonly config.selected
                model.textfield
                config.textfieldConfig
                |> Html.map never
            , Menu.view MenuMsg model.menu items
            , Icon.view "arrow_drop_down"
                [ style
                    [ ( "position", "relative" )
                    , ( "right", "22px" )
                    , ( "top", "7px" )
                    , ( "cursor", "pointer" )
                    ]
                ]
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map MenuMsg (Menu.subscriptions model.menu)
        ]
