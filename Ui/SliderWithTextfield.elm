module Ui.SliderWithTextfield
    exposing
        ( Model
        , Config
        , withLimits
        , defaultModel
        , defaultConfig
        , discretize
        , update
        , view
        , subscriptions
        , Msg(..)
        )

import Html exposing (..)
import Html.Attributes exposing (style, class)
import Ui.Slider as Slider
import Ui.Textfield as Textfield
import Ui.Internal.Textfield as InternalTextfield
import Ui.Internal.Slider as InternalSlider


-- import SliderCss exposing (..)

import FormatNumber exposing (format)
import Utils.General exposing (rusLocale, Plural)


-- ({ class } as class_) =
--     sliderNamespace
--


type alias Model =
    { slider : Slider.Model
    , textfield : Textfield.Model
    }


type alias Config =
    { sliderConfig : Slider.Config
    , textfieldConfig : Textfield.Config
    , extraStatic : Maybe String
    , extraPlural : Maybe Plural
    }


withLimits : Config -> Float -> Float -> Int -> Config
withLimits config min max steps =
    let
        sliderConfig =
            config.sliderConfig

        textfieldConfig =
            config.textfieldConfig

        value =
            if sliderConfig.value > max then
                max
            else if sliderConfig.value < min then
                min
            else
                sliderConfig.value

        updatedSlider =
            { sliderConfig
                | min = min
                , max = max
                , steps = steps
                , value = value
            }
    in
        { config | sliderConfig = updatedSlider }


defaultConfig : Config
defaultConfig =
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


defaultModel : Model
defaultModel =
    { slider = Slider.defaultModel
    , textfield = Textfield.defaultModel
    }


type Msg
    = SliderMsg Slider.Msg
    | TextfieldMsg Textfield.Msg


discretize : Maybe Float -> Slider.Config -> Float
discretize value sliderConfig =
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


onSliderMsg : Slider.Msg -> Model -> Config -> Maybe String -> ( Model, Maybe String )
onSliderMsg msg model { sliderConfig, textfieldConfig } previousInputText =
    let
        ( newSliderModel, _ ) =
            Slider.update msg model.slider

        discretizedValue =
            discretize newSliderModel.value sliderConfig

        ( newTextfieldModel, newText ) =
            Textfield.externalUpdate
                (InternalTextfield.Input
                    (toString
                        discretizedValue
                    )
                )
                model.textfield
                textfieldConfig
                previousInputText
    in
        case msg of
            InternalSlider.MouseDrag pos ->
                ( { model | textfield = newTextfieldModel, slider = newSliderModel }, newText )

            InternalSlider.MouseUp pos ->
                ( { model | textfield = newTextfieldModel, slider = newSliderModel }, newText )

            _ ->
                ( { model | slider = newSliderModel }, previousInputText )


onTextfieldMsg :
    Textfield.Msg
    -> Model
    -> Config
    -> Maybe String
    -> ( Model, Maybe String )
onTextfieldMsg msg model { sliderConfig, textfieldConfig } previousInputText =
    let
        ( newTextfieldModel, newText ) =
            Textfield.externalUpdate msg
                model.textfield
                textfieldConfig
                previousInputText

        discretizedTextfieldValue =
            discretize
                (Just <|
                    toFloat <|
                        (String.toInt
                            (newText |> Maybe.withDefault "0")
                            |> Result.withDefault 0
                        )
                )
                sliderConfig
    in
        case msg of
            InternalTextfield.Blur ->
                let
                    ( newTextfieldModel1, newText ) =
                        Textfield.externalUpdate
                            (InternalTextfield.Input <| toString discretizedTextfieldValue)
                            newTextfieldModel
                            textfieldConfig
                            previousInputText
                in
                    ( { model
                        | textfield = newTextfieldModel1
                      }
                    , newText
                    )

            InternalTextfield.Input str ->
                let
                    ( newTextfieldModel1, newText ) =
                        Textfield.externalUpdate
                            (InternalTextfield.Input <| str)
                            model.textfield
                            textfieldConfig
                            previousInputText

                    ( newSliderModel, _ ) =
                        Slider.update
                            (InternalSlider.SetValue
                                discretizedTextfieldValue
                            )
                            model.slider
                in
                    ( { model
                        | textfield = newTextfieldModel
                        , slider = newSliderModel
                      }
                    , newText
                    )

            _ ->
                ( { model | textfield = newTextfieldModel }, previousInputText )


update : Msg -> Model -> Config -> Maybe String -> ( Model, Maybe String )
update msg model config previousInputText =
    case msg of
        SliderMsg msg_ ->
            onSliderMsg msg_ model config previousInputText

        TextfieldMsg msg_ ->
            onTextfieldMsg msg_ model config previousInputText


view : Maybe String -> Model -> Config -> Html Msg
view inputText model { sliderConfig, textfieldConfig, extraPlural, extraStatic } =
    let
        extra =
            (++) (extraStatic |> Maybe.withDefault "" |> (++) " ")
                (Maybe.map (flip Utils.General.pluralize <| round sliderConfig.min) extraPlural |> Maybe.withDefault "")

        labelMin =
            format rusLocale sliderConfig.min ++ extra

        labelMax =
            format rusLocale sliderConfig.max ++ extra
    in
        Html.div []
            [ div [ style [] ]
                [ div
                    [ style
                        [ ( "width", "368px" )
                        , ( "bottom", "4px" )
                        ]
                    ]
                    [ Textfield.view
                        inputText
                        model.textfield
                        textfieldConfig
                        |> Html.map TextfieldMsg
                    , div
                        [ style [ ( "height", "32px" ) ] ]
                        [ Slider.view
                            model.slider
                            sliderConfig
                            |> Html.map SliderMsg
                        , div [ class "ui-slider-with-textfield-labels-container" ]
                            [ div [ class "ui-slider-with-textfield-label" ] [ text labelMin ]
                            , div [ class "ui-slider-with-textfield-label" ] [ text labelMax ]
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
