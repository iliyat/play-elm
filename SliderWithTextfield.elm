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


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        SliderMsg msg_ ->
            let
                ( slider, effects ) =
                    Slider.update SliderMsg msg_ model.slider

                ( textfield, textfieldEffects ) =
                    case msg_ of
                        Internal.Slider.Input val ->
                            Textfield.update (TextfieldMsg) (Internal.Textfield.Input (toString val)) model.textfield

                        _ ->
                            ( model.textfield, Cmd.none )
            in
                ( { model | slider = slider, textfield = textfield }
                , Cmd.batch
                    [ effects, textfieldEffects ]
                )

        TextfieldMsg msg_ ->
            let
                ( newTextfieldModel, _ ) =
                    case msg_ of
                        Internal.Textfield.Blur ->
                            let
                                { max, steps } =
                                    sliderConfig

                                discretized =
                                    Slider.discretize sliderConfig.steps (model.slider.value |> Maybe.withDefault 0)

                                result =
                                    if discretized > max then
                                        max
                                    else
                                        discretized
                            in
                                Textfield.update (TextfieldMsg)
                                    (Internal.Textfield.Input (toString result))
                                    model.textfield

                        _ ->
                            ( model.textfield, Cmd.none )

                ( newSliderModel, sliderEffects ) =
                    case msg_ of
                        Internal.Textfield.Input str ->
                            let
                                textFieldValue =
                                    String.toInt str |> Result.withDefault 0

                                targetValue =
                                    Slider.discretize sliderConfig.steps (toFloat textFieldValue)
                            in
                                Slider.update SliderMsg (Internal.Slider.SetValue (targetValue)) model.slider

                        _ ->
                            ( model.slider, Cmd.none )

                ( textfield, effects ) =
                    Textfield.update TextfieldMsg msg_ newTextfieldModel
            in
                ( { model | textfield = textfield, slider = newSliderModel }, Cmd.batch [ sliderEffects, effects ] )


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
            [ div [ style [ ( "margin", "24px" ) ] ]
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
