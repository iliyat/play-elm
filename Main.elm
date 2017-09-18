module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Slider
import Dict exposing (Dict)
import Menu
import Textfield


type alias Model =
    { slider : Slider.Model
    , menu : Menu.Model
    , textfield : Textfield.Model
    }


defaultModel : Model
defaultModel =
    { slider = Slider.defaultModel
    , menu = Menu.defaultModel
    , textfield = Textfield.defaultModel
    }


type Msg
    = Open
    | SliderMsg (Slider.Msg Msg)
    | MenuMsg (Menu.Msg Msg)
    | TextfieldMsg Textfield.Msg
    | Select Int


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        SliderMsg msg_ ->
            let
                ( slider, effects ) =
                    Slider.update SliderMsg msg_ model.slider
            in
                ( { model | slider = slider }, effects )

        MenuMsg msg_ ->
            let
                ( menu, effects ) =
                    Menu.update MenuMsg msg_ model.menu
            in
                ( { model | menu = menu }, effects )

        TextfieldMsg msg_ ->
            let
                ( textfield, effects ) =
                    Textfield.update TextfieldMsg msg_ model.textfield
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
            [ div [ style [ ( "width", "200px" ), ( "border", "1px solid cyan" ) ] ]
                [ Slider.view SliderMsg model.slider
                ]
            , div [ style [ ( "height", "50px" ) ] ] []
            , button [ Menu.attach MenuMsg ] [ text "Toggle!" ]
            , Menu.view MenuMsg
                model.menu
                ([ li [ class "mdc-list-item", Menu.onSelect (Select 1) ] [ text "Редактировать" ]
                 , li [ class "mdc-list-item" ] [ text "Отправить в архив" ]
                 ]
                )
            , div [ style [ ( "height", "50px" ) ] ] []
            , Textfield.view TextfieldMsg model.textfield
            ]
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
        , Sub.map SliderMsg (Slider.subscriptions model.slider)
        ]
