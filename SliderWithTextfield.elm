module SliderWithTextfield exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style, class)
import Slider
import Textfield
import Internal.Textfield
import Internal.Slider
import SliderCss exposing (..)
import FormatNumber.Locales exposing (Locale)
import FormatNumber exposing (format)
import Utils exposing (..)


rusLocale : Locale
rusLocale =
    Locale 0 " " "." "-" ""


({ class } as class_) =
    sliderNamespace


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


onSliderMsg : Slider.Msg -> Model -> Config -> Model
onSliderMsg msg model { sliderConfig, textfieldConfig } =
    let
        ( newSliderModel, _ ) =
            Slider.update SliderMsg msg model.slider
    in
        case msg of
            Internal.Slider.MouseDrag pos ->
                let
                    discretizedValue =
                        discretize newSliderModel.value sliderConfig

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


onTextfieldMsg : Textfield.Msg -> Model -> Config -> Model
onTextfieldMsg msg model { sliderConfig, textfieldConfig } =
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
                sliderConfig
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
                        Slider.update SliderMsg
                            (Internal.Slider.SetValue
                                discretizedTextfieldValue
                            )
                            model.slider
                in
                    ({ model
                        | textfield = newTextfieldModel
                        , slider =
                            newSliderModel
                     }
                    )

            _ ->
                ({ model | textfield = newTextfieldModel })


update : (Msg -> m) -> Msg -> Model -> Config -> ( Model, Cmd m )
update lift msg model config =
    case msg of
        SliderMsg msg_ ->
            ( onSliderMsg msg_ model config, Cmd.none )

        TextfieldMsg msg_ ->
            ( onTextfieldMsg msg_ model config, Cmd.none )


view : (Msg -> m) -> Model -> Config -> Html m
view lift model { sliderConfig, textfieldConfig, extraPlural, extraStatic } =
    let
        extra =
            (++) (extraStatic |> Maybe.withDefault "" |> (++) " ")
                (Maybe.map (flip Utils.pluralize <| round sliderConfig.min) extraPlural |> Maybe.withDefault "")

        labelMin =
            format rusLocale sliderConfig.min ++ extra

        labelMax =
            format rusLocale sliderConfig.max ++ extra
    in
        Html.div []
            [ div [ style [] ]
                [ div [ style [ ( "width", "368px" ) ] ]
                    [ Textfield.view
                        model.textfield
                        textfieldConfig
                        |> Html.map (lift << TextfieldMsg)
                    , div
                        []
                        [ Slider.view (lift << SliderMsg)
                            model.slider
                            sliderConfig
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
