module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Slider
import Dict exposing (Dict)


type alias Model =
    { slider : Slider.Model
    }


defaultModel : Model
defaultModel =
    { slider = Slider.defaultModel }


type Msg
    = Open
    | SliderMsg (Slider.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        SliderMsg msg_ ->
            let
                ( slider, effects ) =
                    Slider.update SliderMsg msg_ model.slider
            in
                ( { model | slider = slider }, effects )

        Open ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div []
        [ div [ style [ ( "margin", "24px" ) ] ]
            [ Slider.view SliderMsg model.slider
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        []


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
