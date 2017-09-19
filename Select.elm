module Select exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style, class)
import Textfield
import Internal.Menu
import Icons.Icon as Icon
import Html.Events as Events
import Menu


type alias Model =
    { textfield : Textfield.Model
    , menu : Menu.Model
    }


defaultModel : Model
defaultModel =
    { textfield = Textfield.defaultModel
    , menu = Menu.defaultModel
    }


type Msg
    = TextfieldMsg Textfield.Msg
    | MenuMsg Menu.Msg


update : (Msg -> m) -> Msg -> Model -> ( Model, Cmd m )
update lift msg model =
    case msg of
        TextfieldMsg msg_ ->
            let
                ( newTextfieldModel, _ ) =
                    Textfield.update TextfieldMsg msg_ model.textfield textfieldConfig
            in
                ( { model | textfield = newTextfieldModel }, Cmd.none )

        MenuMsg msg_ ->
            let
                ( m, _ ) =
                    Menu.update MenuMsg msg_ model.menu
            in
                ( { model | menu = m }, Cmd.none )



-- _ ->
--     ( model, Cmd.none )


type alias Config =
    { items : List String
    }


defaultConfig : Config
defaultConfig =
    { items = [] }


textfieldConfig : Textfield.Config
textfieldConfig =
    let
        dc =
            Textfield.defaultConfig
    in
        { dc
            | fullWidth = False
            , readonly = True
            , defaultValue = Just "ololo"
        }


view : (Msg -> m) -> Model -> Config -> Html m
view lift model config =
    let
        labelMin =
            2
    in
        div
            [ Events.onClick (lift <| MenuMsg Internal.Menu.Open)
            , style
                [ ( "position", "relative" )
                , ( "width", "198px" )
                , ( "height", "100%" )
                , ( "display", "flex" )
                , ( "align-items", "center" )
                , ( "cursor", "pointer" )
                ]
            ]
            [ Menu.view (lift << MenuMsg)
                model.menu
                ([ li [ class "mdc-list-item" ] [ text "Редактировать" ]
                 , li [ class "mdc-list-item" ] [ text "Отправить в архив" ]
                 ]
                )
            , Textfield.view
                (lift << TextfieldMsg)
                model.textfield
                textfieldConfig
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
