module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Slider
import Textfield
import Internal.Textfield
import Internal.Slider


type alias Model =
    { slider : Slider.Model
    , textfield : Textfield.Model
    }


defaultModel : Model
defaultModel =
    { slider = Slider.defaultModel
    , textfield = Textfield.defaultModel
    }


type Msg
    = SliderMsg (Slider.Msg Msg)
    | TextfieldMsg Textfield.Msg


discretize : Maybe Float -> Float
discretize value =
    let
        discretizedValue =
            Slider.discretize sliderConfig.steps (value |> Maybe.withDefault 0)
    in
        if discretizedValue > sliderConfig.max then
            sliderConfig.max
        else
            discretizedValue


onSliderMsg : Slider.Msg Msg -> Model -> Model
onSliderMsg msg model =
    let
        ( newSliderModel, _ ) =
            Slider.update SliderMsg msg model.slider
    in
        case msg of
            Internal.Slider.MouseDrag pos ->
                let
                    discretizedValue =
                        discretize newSliderModel.value

                    ( newTextfieldModel, _ ) =
                        Textfield.update TextfieldMsg (Internal.Textfield.Input (toString discretizedValue)) model.textfield
                in
                    ({ model | textfield = newTextfieldModel, slider = newSliderModel })

            _ ->
                ({ model | slider = newSliderModel })


onTextfieldMsg : Textfield.Msg -> Model -> Model
onTextfieldMsg msg model =
    let
        ( newTextfieldModel, _ ) =
            Textfield.update TextfieldMsg msg model.textfield

        discretizedTextfieldValue =
            discretize
                (Just <|
                    toFloat <|
                        (String.toInt
                            (newTextfieldModel.value |> Maybe.withDefault "0")
                            |> Result.withDefault 0
                        )
                )
    in
        case msg of
            Internal.Textfield.Blur ->
                let
                    ( newTextfieldModel1, _ ) =
                        Textfield.update TextfieldMsg
                            (Internal.Textfield.Input <| toString discretizedTextfieldValue)
                            newTextfieldModel
                in
                    ({ model | textfield = newTextfieldModel1 })

            Internal.Textfield.Input str ->
                let
                    ( newSliderModel, _ ) =
                        Slider.update SliderMsg (Internal.Slider.SetValue discretizedTextfieldValue) model.slider
                in
                    ({ model
                        | textfield = newTextfieldModel
                        , slider =
                            newSliderModel
                     }
                    )

            _ ->
                ({ model | textfield = newTextfieldModel })


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        SliderMsg msg_ ->
            ( onSliderMsg msg_ model, Cmd.none )

        TextfieldMsg msg_ ->
            ( onTextfieldMsg msg_ model, Cmd.none )


sliderConfig : Slider.Config
sliderConfig =
    { value = 0
    , min = 0
    , max = 20
    , steps = 5
    , discrete = False
    , trackMarkers = False
    }


textfieldConfig : Textfield.Config
textfieldConfig =
    { labelText = Just "Сумма"
    , labelFloat = False
    , value = Nothing
    , defaultValue = Nothing
    , disabled = False
    , asTitle = True
    , required = False
    , type_ = Just "text"
    , fullWidth = False
    , invalid = False
    , extra = Just "₽"
    }


view : Model -> Html Msg
view model =
    let
        sc =
            { sliderConfig
                | value = 1
            }
    in
        Html.div []
            [ Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "mdc.css" ] []
            , Html.node "script" [ Html.Attributes.src "mdc.js" ] []
            , div [ style [ ( "margin", "24px" ) ] ]
                [ div [ style [ ( "width", "368px" ) ] ]
                    [ Textfield.view TextfieldMsg
                        model.textfield
                        textfieldConfig
                    , div [ style [ ( "position", "relative" ), ( "bottom", "16px" ) ] ]
                        [ Slider.view SliderMsg model.slider sc
                        ]
                    ]
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
        [ Sub.map SliderMsg (Slider.subscriptions model.slider)
        ]
