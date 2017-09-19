module SliderWithTextfield exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Slider
import Textfield
import Internal.Textfield
import Internal.Slider
import SliderCss exposing (..)
import FormatNumber.Locales exposing (Locale)
import FormatNumber exposing (format)


rusLocale : Locale
rusLocale =
    Locale 0 " " "." "-" ""


({ class } as class_) =
    sliderNamespace


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
    = SliderMsg Slider.Msg
    | TextfieldMsg Textfield.Msg


discretize : Maybe Float -> Float
discretize value =
    let
        discretizedValue =
            Slider.discretize sliderConfig.steps (value |> Maybe.withDefault 0)
    in
        if discretizedValue > sliderConfig.max then
            sliderConfig.max
        else if discretizedValue < sliderConfig.min then
            sliderConfig.min
        else
            discretizedValue


onSliderMsg : Slider.Msg -> Model -> Model
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
                        Textfield.update
                            TextfieldMsg
                            (Internal.Textfield.Input (toString discretizedValue))
                            model.textfield
                            textfieldConfig
                in
                    ({ model | textfield = newTextfieldModel, slider = newSliderModel })

            _ ->
                ({ model | slider = newSliderModel })


onTextfieldMsg : Textfield.Msg -> Model -> Model
onTextfieldMsg msg model =
    let
        ( newTextfieldModel, _ ) =
            Textfield.update TextfieldMsg msg model.textfield textfieldConfig

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
                            textfieldConfig
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


update : (Msg -> m) -> Msg -> Model -> ( Model, Cmd m )
update lift msg model =
    case msg of
        SliderMsg msg_ ->
            ( onSliderMsg msg_ model, Cmd.none )

        TextfieldMsg msg_ ->
            ( onTextfieldMsg msg_ model, Cmd.none )


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
        { dc
            | defaultValue = Just "2000"
            , extra = Just "₽"
            , fullWidth = True
            , asTitle = True
            , numbered = True
        }


view : (Msg -> m) -> Model -> Html m
view lift model =
    let
        sc =
            { sliderConfig
                | value = 1
            }

        labelMin =
            format rusLocale sliderConfig.min ++ " ₽"

        labelMax =
            format rusLocale sliderConfig.max ++ " ₽"
    in
        Html.div []
            [ div [ style [ ( "margin", "24px" ) ] ]
                [ div [ style [ ( "width", "368px" ) ] ]
                    [ Textfield.view (lift << TextfieldMsg) model.textfield textfieldConfig
                    , div [ style [ ( "position", "relative" ), ( "bottom", "8px" ) ] ]
                        [ Slider.view (lift << SliderMsg) model.slider sc
                        , div [ class [ LabelsContainer ] ]
                            [ div [ class [ Label ] ] [ text labelMin ]
                            , div [ class [ Label ] ] [ text labelMax ]
                            ]
                        ]
                    ]
                ]
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map SliderMsg (Slider.subscriptions model.slider)
        ]
