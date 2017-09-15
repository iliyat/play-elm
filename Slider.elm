module Slider exposing (view, Model, defaultModel, Msg, update)

import Html exposing (Html, text, div, button, Attribute, ul, li)
import Html.Attributes as Attr exposing (class, classList, style)
import Html.Events as Events
import Json.Decode as Json exposing (Decoder)
import Svg
import Svg.Attributes as Svg
import Internal.Slider exposing (Msg(..), Geometry, defaultGeometry)
import DOM


type alias Model =
    { focus : Bool
    , active : Bool
    , geometry : Maybe Geometry
    , value : Maybe Float
    , inTransit : Bool
    , initialized : Bool
    , requestAnimation : Bool
    }


defaultModel : Model
defaultModel =
    { focus = False
    , active = False
    , geometry = Nothing
    , value = Nothing
    , inTransit = False
    , initialized = False
    , requestAnimation = True
    }


type alias Msg m =
    Internal.Slider.Msg m


update : (Msg m -> m) -> Msg m -> Model -> ( Model, Cmd m )
update fwd msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Focus ->
            ( { model | focus = True }, Cmd.none )

        Blur ->
            ( { model | focus = False, active = False }, Cmd.none )

        Activate inTransit geometry ->
            let
                _ =
                    Debug.log "test" "test"
            in
                ( { model
                    | active = True
                    , geometry = Just geometry
                    , inTransit = inTransit
                    , value = Just (computeValue geometry)
                  }
                , Cmd.none
                )

        Drag geometry ->
            ( { model
                | geometry = Just geometry
                , inTransit = False
                , value = Just (computeValue geometry)
              }
            , Cmd.none
            )

        Init geometry ->
            ( { model
                | geometry = Just geometry
                , value = Just (computeValue geometry)
                , initialized = True
              }
            , Cmd.none
            )

        Up ->
            ( { model | active = False }, Cmd.none )

        Resize ->
            ( { model | requestAnimation = True }
            , Cmd.none
            )

        AnimationFrame ->
            ( model, Cmd.none )

        Tick ->
            ( model, Cmd.none )

        Dispatch _ ->
            ( model, Cmd.none )


type alias Config m =
    { value : Float
    , min : Float
    , max : Float
    , discrete : Bool
    , steps : Int
    , onInput : Maybe (Decoder m)
    , onChange : Maybe (Decoder m)
    , trackMarkers : Bool
    }


defaultConfig : Config m
defaultConfig =
    { value = 0
    , min = 0
    , max = 100
    , steps = 1
    , discrete = False
    , onInput = Nothing
    , onChange = Nothing
    , trackMarkers = False
    }


discretize : Int -> Float -> Float
discretize steps continuousValue =
    toFloat (steps * round (continuousValue / toFloat steps))


computeValue : Geometry -> Float
computeValue geometry =
    let
        c =
            if geometry.width /= 0 then
                (geometry.x - geometry.left) / geometry.width
            else
                0
                    |> clamp 0 1
    in
        geometry.min
            + c
            * (geometry.max - geometry.min)
            |> clamp geometry.min geometry.max


decodeGeometry : Decoder Geometry
decodeGeometry =
    Json.oneOf
        [ Json.at [ "pageX" ] Json.float
        , Json.succeed 0
        ]
        |> Json.andThen
            (\x ->
                DOM.target <|
                    traverseToContainer <|
                        Json.map6
                            (\offsetWidth offsetLeft discrete min max steps ->
                                { width = offsetWidth
                                , left = offsetLeft
                                , x = x
                                , discrete = discrete
                                , min = min
                                , max = max
                                , steps = steps
                                }
                            )
                            DOM.offsetWidth
                            DOM.offsetLeft
                            (hasClass "mdc-slider--discrete")
                            (data "min" (Json.map (String.toFloat >> Result.withDefault 1) Json.string))
                            (data "max" (Json.map (String.toFloat >> Result.withDefault 1) Json.string))
                            (data "steps" (Json.map (String.toInt >> Result.withDefault 1) Json.string))
            )


data : String -> Decoder a -> Decoder a
data key decoder =
    Json.at [ "dataset", key ] decoder


traverseToContainer : Decoder a -> Decoder a
traverseToContainer decoder =
    hasClass "mdc-slider"
        |> Json.andThen
            (\doesHaveClass ->
                if doesHaveClass then
                    decoder
                else
                    DOM.parentElement (Json.lazy (\_ -> traverseToContainer decoder))
            )


hasClass : String -> Decoder Bool
hasClass class =
    Json.map
        (\className ->
            String.contains (" " ++ class ++ " ") (" " ++ className ++ " ")
        )
        (Json.at [ "className" ] Json.string)


onMouseDown : (Geometry -> msg) -> Attribute msg
onMouseDown msg =
    Events.onWithOptions "mousedown"
        { stopPropagation = True
        , preventDefault = False
        }
        (Json.map msg decodeGeometry)


dataAttr : String -> String -> Attribute msg
dataAttr key val =
    Attr.attribute ("data-" ++ key) val


view : (Msg m -> m) -> Model -> Html m
view lift model =
    let
        config =
            defaultConfig

        continuousValue =
            if model.active then
                model.value
                    |> Maybe.withDefault config.value
            else
                config.value

        value =
            discretize config.steps continuousValue

        translateX =
            let
                v =
                    value
                        |> clamp config.min config.max

                c =
                    if (config.max - config.min) /= 0 then
                        (v - config.min)
                            / (config.max - config.min)
                            |> clamp 0 1
                    else
                        0
            in
                model.geometry
                    |> Maybe.map .width
                    |> Maybe.map ((*) c)
                    |> Maybe.withDefault 0

        activateOn event =
            Events.on event (Json.map (Activate True >> lift) decodeGeometry)

        ups =
            [ "mouseup"
            , "touchend"
            , "pointerup"
            ]

        downs =
            [ "mousedown"
            , "touchstart"
            , "keydown"
            , "pointerdown"
            ]

        moves =
            [ "mousemove"
            , "touchmove"
            , "pointermove"
            ]

        leaves =
            [ "mouseleave"
            , "touchleave" -- TODO
            , "pointerleave" -- TODO
            ]
    in
        div
            ([ classList
                [ ( "mdc-slider mdc-slider--discrete", True )
                , ( "mdc-slider--focus", model.focus )
                , ( "mdc-slider--active", model.active )
                , ( "mdc-slider--in-transit", model.inTransit )
                ]
             , dataAttr "min" (toString config.min)
             , dataAttr "max" (toString config.max)
             , dataAttr "steps" (toString config.steps)
             , Events.on "focus" (Json.succeed (lift Focus))
             , Events.on "blur" (Json.succeed (lift Blur))
             , onMouseDown (Activate False >> lift)

             -- , Events.on "mousedown" (Json.succeed <| lift TestClick)
             ]
                ++ List.map activateOn downs
            )
            [ div [ class "mdc-slider__track-container" ]
                [ div
                    [ class "mdc-slider__track"
                    , style [ ( "transform", "scaleX(0.35)" ) ]
                    ]
                    []
                , div [ class "mdc-slider__track-marker-container" ]
                    (List.repeat ((round (config.max - config.min)) // config.steps) <|
                        div [ class "mdc-slider__track-marker" ] []
                    )
                ]
            , div
                [ class "mdc-slider__thumb-container"
                , onMouseDown (Activate False >> lift)
                , style
                    [ ( "transform", "translateX(" ++ toString translateX ++ "px) translateX(-50%)" )
                    ]
                ]
                [ Svg.svg
                    [ Svg.class "mdc-slider__thumb"
                    , Svg.width "21"
                    , Svg.height "21"
                    ]
                    [ Svg.circle
                        [ Svg.cx "10.5"
                        , Svg.cy "10.5"
                        , Svg.r "7.875"
                        ]
                        []
                    ]
                , div
                    [ class "mdc-slider__focus-ring" ]
                    []
                , div
                    [ class "mdc-slider__pin" ]
                    [ div
                        [ class "mdc-slider__pin-value-marker" ]
                        [ text (toString value) ]
                    ]
                ]
            ]
