module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Slider
import Textfield
import Select
import SliderWithTextfield
import Utils exposing (..)


type alias Model =
    { textfield : Textfield.Model
    , select : Select.Model
    , sliderWithTextfield1 : SliderWithTextfield.Model
    , sliderWithTextfield2 : SliderWithTextfield.Model
    }


defaultModel : Model
defaultModel =
    { textfield = Textfield.defaultModel
    , select = Select.defaultModel
    , sliderWithTextfield1 = SliderWithTextfield.defaultModel
    , sliderWithTextfield2 = SliderWithTextfield.defaultModel
    }


type Msg
    = Open
    | SliderWithTextfieldMsg1 SliderWithTextfield.Msg
    | SliderWithTextfieldMsg2 SliderWithTextfield.Msg
    | TextfieldMsg Textfield.Msg
    | SelectMsg Select.Msg
    | Select Int


swtConf1 : SliderWithTextfield.Config
swtConf1 =
    let
        slider =
            Slider.defaultConfig

        textfield =
            Textfield.defaultConfig
    in
        { sliderConfig =
            { slider
                | value = 2000
                , min = 2000
                , max = 10000
                , steps = 1000
            }
        , textfieldConfig =
            { textfield
                | defaultValue = Just "2000"
                , asTitle = True
                , numbered = True
                , extra = Just "₽"
                , fullWidth = True
                , labelText = Just "Сумма"
            }
        , extraStatic = Just "₽"
        , extraPlural = Nothing
        }


swtConf2 : SliderWithTextfield.Config
swtConf2 =
    let
        slider =
            Slider.defaultConfig

        textfield =
            Textfield.defaultConfig
    in
        { sliderConfig =
            { slider
                | value = 7
                , min = 7
                , max = 20
                , steps = 1
            }
        , textfieldConfig =
            { textfield
                | defaultValue = Just "7"
                , asTitle = True
                , numbered = True
                , plural = Just (Plural "день" "дня" "дней")
                , fullWidth = True
                , labelText = Just "Срок"
            }
        , extraPlural = Just (Plural "день" "дня" "дней")
        , extraStatic = Nothing
        }


textfieldConfig : Textfield.Config
textfieldConfig =
    let
        dc =
            Textfield.defaultConfig
    in
        { dc
            | defaultValue = Just "Промокод"
            , readonly = False
            , labelText = Just "Промокод"
        }



-- selectConfig : Select.Config
-- selectConfig =
--     let
--         dc =
--             Select.defaultConfig
--     in
--         dc
--


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        SliderWithTextfieldMsg1 msg_ ->
            let
                ( new, effects ) =
                    SliderWithTextfield.update SliderWithTextfieldMsg1
                        msg_
                        model.sliderWithTextfield1
                        swtConf1
            in
                ( { model | sliderWithTextfield1 = new }, effects )

        SliderWithTextfieldMsg2 msg_ ->
            let
                ( new, effects ) =
                    SliderWithTextfield.update SliderWithTextfieldMsg2
                        msg_
                        model.sliderWithTextfield2
                        swtConf2
            in
                ( { model | sliderWithTextfield2 = new }, effects )

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
            [ SliderWithTextfield.view SliderWithTextfieldMsg1
                model.sliderWithTextfield1
                swtConf1
            , SliderWithTextfield.view SliderWithTextfieldMsg2
                model.sliderWithTextfield2
                swtConf2
            , Textfield.view TextfieldMsg model.textfield textfieldConfig

            -- , Select.view SelectMsg model.select selectConfig
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
        , Sub.map SliderWithTextfieldMsg1
            (SliderWithTextfield.subscriptions
                model.sliderWithTextfield1
            )
        , Sub.map SliderWithTextfieldMsg2
            (SliderWithTextfield.subscriptions
                model.sliderWithTextfield2
            )
        ]
