module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Slider
import Textfield
import Select
import SliderWithTextfield


type alias Model =
    { textfield : Textfield.Model
    , select : Select.Model
    , sliderWithTextfield : SliderWithTextfield.Model
    }


defaultModel : Model
defaultModel =
    { textfield = Textfield.defaultModel
    , select = Select.defaultModel
    , sliderWithTextfield = SliderWithTextfield.defaultModel
    }


type Msg
    = Open
    | SliderWithTextfieldMsg SliderWithTextfield.Msg
    | TextfieldMsg Textfield.Msg
    | SelectMsg Select.Msg
    | Select Int


sliderConfig1 : Slider.Config
sliderConfig1 =
    let
        sc =
            Slider.defaultConfig
    in
        { sc | value = 0, min = 2000, max = 10000, steps = 1000 }


textfieldConfig : Textfield.Config
textfieldConfig =
    let
        dc =
            Textfield.defaultConfig
    in
        { dc | defaultValue = Just "ololo", readonly = True }


selectConfig : Select.Config
selectConfig =
    let
        dc =
            Select.defaultConfig
    in
        dc


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        SliderWithTextfieldMsg msg_ ->
            let
                ( new, effects ) =
                    SliderWithTextfield.update SliderWithTextfieldMsg
                        msg_
                        model.sliderWithTextfield
                        sliderConfig1
            in
                ( { model | sliderWithTextfield = new }, effects )

        TextfieldMsg msg_ ->
            let
                ( textfield, effects ) =
                    Textfield.update TextfieldMsg
                        msg_
                        model.textfield
                        textfieldConfig
            in
                ( { model | textfield = textfield }, effects )

        SelectMsg msg_ ->
            let
                ( select, effects ) =
                    Select.update SelectMsg msg_ model.select
            in
                ( { model | select = select }, effects )

        Select n ->
            ( model, Cmd.none )

        Open ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div []
        [ div [ style [ ( "margin", "54px" ) ] ]
            [ SliderWithTextfield.view SliderWithTextfieldMsg
                model.sliderWithTextfield
                sliderConfig1

            -- , Textfield.view TextfieldMsg model.textfield textfieldConfig
            , Select.view SelectMsg model.select selectConfig
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
        [ Sub.map SelectMsg (Select.subscriptions model.select)
        , Sub.map SliderWithTextfieldMsg (SliderWithTextfield.subscriptions model.sliderWithTextfield)
        ]
