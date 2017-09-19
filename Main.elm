module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Slider
import Menu
import Textfield
import SliderWithTextfield


type alias Model =
    { menu : Menu.Model
    , textfield : Textfield.Model
    , sliderWithTextfield : SliderWithTextfield.Model
    }


defaultModel : Model
defaultModel =
    { menu = Menu.defaultModel
    , textfield = Textfield.defaultModel
    , sliderWithTextfield = SliderWithTextfield.defaultModel
    }


type Msg
    = Open
    | SliderWithTextfieldMsg SliderWithTextfield.Msg
    | TextfieldMsg Textfield.Msg
    | MenuMsg (Menu.Msg Msg)
    | Select Int


sliderConfig : Slider.Config
sliderConfig =
    let
        sc =
            Slider.defaultConfig
    in
        { sc | value = 2000, min = 2000, max = 10000, steps = 1000 }


textfieldConfig : Textfield.Config
textfieldConfig =
    let
        dc =
            Textfield.defaultConfig
    in
        dc


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        SliderWithTextfieldMsg msg_ ->
            let
                ( new, effects ) =
                    SliderWithTextfield.update SliderWithTextfieldMsg msg_ model.sliderWithTextfield
            in
                ( { model | sliderWithTextfield = new }, effects )

        MenuMsg msg_ ->
            let
                ( menu, effects ) =
                    Menu.update MenuMsg msg_ model.menu
            in
                ( { model | menu = menu }, effects )

        TextfieldMsg msg_ ->
            let
                ( textfield, effects ) =
                    Textfield.update TextfieldMsg
                        msg_
                        model.textfield
                        textfieldConfig
            in
                ( { model | textfield = textfield }, effects )

        Select n ->
            ( model, Cmd.none )

        Open ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div []
        [ div [ style [ ( "margin", "24px" ) ] ]
            [ SliderWithTextfield.view SliderWithTextfieldMsg model.sliderWithTextfield
            , Textfield.view TextfieldMsg model.textfield textfieldConfig
            ]
        , Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "mdc.css" ] []
        , Html.node "script" [ Html.Attributes.src "mdc.js" ] []
        , Html.node "link"
            [ Html.Attributes.rel "stylesheet"
            , Html.Attributes.href "https://unpkg.com/material-components-web@latest/dist/material-components-web.min.css"
            ]
            []
        , Html.node "link"
            [ Html.Attributes.rel "stylesheet"
            , Html.Attributes.href "https://fonts.googleapis.com/icon?family=Material+Icons"
            ]
            []
        , Html.node "link"
            [ Html.Attributes.rel "stylesheet", Html.Attributes.href "https://fonts.googleapis.com/css?family=Roboto:300,400,500" ]
            []
        , Html.node "link"
            [ Html.Attributes.rel "stylesheet"
            , Html.Attributes.href
                "slider.css"
            ]
            []
        ]


init : ( Model, Cmd Msg )
init =
    ( defaultModel, Cmd.none )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map MenuMsg (Menu.subscriptions model.menu)
        , Sub.map SliderWithTextfieldMsg
            (SliderWithTextfield.subscriptions
                model.sliderWithTextfield
            )
        ]
