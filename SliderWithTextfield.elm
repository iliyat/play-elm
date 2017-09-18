module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Slider
import Dict exposing (Dict)
import Menu
import Textfield
import Json.Decode as Json exposing (Decoder)
import Internal.Textfield
import Internal.Slider


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
    | TextfieldMsg Textfield.Msg
    | Select Int
    | OnSliderInput Float
    | OnInputChange String
    | Change Float


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        SliderMsg msg_ ->
            let
                ( slider, effects ) =
                    Slider.update SliderMsg msg_ model.slider
            in
                ( { model | slider = slider }, effects )

        TextfieldMsg msg_ ->
            let
                ( newSliderModel, sliderEffects ) =
                    case msg_ of
                        Internal.Textfield.Input str ->
                            let
                                result =
                                    String.toInt str |> Result.withDefault 0
                            in
                                Slider.update SliderMsg
                                    (Internal.Slider.SetValue
                                        (toFloat result)
                                    )
                                    model.slider

                        _ ->
                            ( model.slider, Cmd.none )

                ( textfield, effects ) =
                    Textfield.update TextfieldMsg msg_ model.textfield
            in
                ( { model | textfield = textfield, slider = newSliderModel }, Cmd.batch [ sliderEffects, effects ] )

        Select n ->
            ( model, Cmd.none )

        Open ->
            ( model, Cmd.none )

        OnSliderInput val ->
            let
                _ =
                    Debug.log "input" "input"

                ( textfield, effects ) =
                    Textfield.update (TextfieldMsg) (Internal.Textfield.Input (toString val)) model.textfield
            in
                ( { model | textfield = textfield }, effects )

        Change val ->
            ( model, Cmd.none )

        OnInputChange str ->
            ( model, Cmd.none )


sliderConfig : Slider.Config Msg
sliderConfig =
    { value = 5
    , min = 0
    , max = 20
    , steps = 5
    , discrete = False
    , onInput = OnSliderInput
    , onChange = Change
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
