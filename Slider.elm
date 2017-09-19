module Slider
    exposing
        ( view
        , Model
        , defaultModel
        , Msg
        , update
        , subscriptions
        , Config
        , targetValue
        , onInput
        , discretize
        )

import Html exposing (Html, text, div, button, Attribute, ul, li)
import Html.Attributes as Attr exposing (tabindex, class, classList, style)
import Html.Events as Events
import Json.Decode as Json exposing (Decoder)
import Svg
import Svg.Attributes as Svg
import Internal.Slider exposing (Msg(..), Geometry, defaultGeometry)
import DOM
import Mouse


onInput : Decoder m -> Attribute m
onInput =
    Events.on "input"


subscriptions : Model -> Sub (Msg m)
subscriptions model =
    let
        list =
            [ Mouse.ups MouseUp ]
                ++ (if model.active == True then
                        [ Mouse.moves MouseDrag ]
                    else
                        []
                   )
    in
        Sub.batch list


type alias Model =
    { focus : Bool
    , active : Bool
    , touched : Bool
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
    , touched = False
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

        Input val ->
            let
                _ =
                    Debug.log "Input" val
            in
                ( model, Cmd.none )

        SetValue val ->
            ( { model | value = Just val }, Cmd.none )

        Dispatch ms ->
            ( model, Cmd.none )

        Focus ->
            ( { model | focus = True }, Cmd.none )

        Blur ->
            ( { model | focus = False, active = False }, Cmd.none )

        Tick ->
            ( { model | inTransit = False }, Cmd.none )

        Activate inTransit geometry ->
            ( { model
                | active = True
                , geometry = Just geometry
                , inTransit = inTransit
                , value = Just (computeValue geometry)
              }
            , Cmd.none
            )

        Drag geometry ->
            ( model, Cmd.none )

        Init geometry ->
            ( { model
                | geometry = Just geometry
                , value = Just (computeValue geometry)
                , initialized = True
              }
            , Cmd.none
            )

        Up ->
            ( model
            , Cmd.none
            )

        MouseUp pos ->
            ( { model
                | active = False
              }
            , Cmd.none
            )

        MouseDrag pos ->
            let
                g =
                    model.geometry |> Maybe.withDefault defaultGeometry

                newGeometry =
                    { g | x = (toFloat pos.x) }

                value =
                    Just (computeValue newGeometry)
            in
                ( { model
                    | geometry = Just newGeometry
                    , value = value
                  }
                , Cmd.none
                )

        Resize ->
            ( { model | requestAnimation = True }
            , Cmd.none
            )

        AnimationFrame ->
            if model.requestAnimation then
                ( { model
                    | requestAnimation = False
                    , initialized = False
                  }
                , Cmd.none
                )
            else
                ( model, Cmd.none )


type alias Config =
    { value : Float
    , min : Float
    , max : Float
    , discrete : Bool
    , steps : Int
    , trackMarkers : Bool
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


targetValue : Decoder Float
targetValue =
    Json.map
        (\geometry ->
            if geometry.discrete then
                discretize geometry.steps (computeValue geometry)
            else
                computeValue geometry
        )
        decodeGeometry


view : (Msg m -> m) -> Model -> Config -> Html m
view lift model config =
    let
        continuousValue =
            model.value
                |> Maybe.withDefault config.value

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

        initOn event =
            Events.on event (Json.map (Init >> lift) decodeGeometry)

        upOn event =
            Events.on event (Json.succeed (lift Up))

        dragOn event =
            Events.on event (Json.map (Drag >> lift) decodeGeometry)

        inputOn event =
            Events.on event (Json.map (Input >> lift) targetValue)

        -- Events.on event (Maybe.withDefault (Json.succeed (lift NoOp)) config.onInput)
        changeOn event =
            Events.on event (Json.map (Input >> lift) targetValue)

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

        activateOn_ event =
            Events.onWithOptions event
                { stopPropagation = True
                , preventDefault = False
                }
                (Json.map (Activate False >> lift) decodeGeometry)

        trackScale =
            if config.max - config.min == 0 then
                0
            else
                (value - config.min) / (config.max - config.min)
    in
        div
            ([ classList
                [ ( "mdc-slider mdc-slider--discrete elm-mdc-slider--uninitialized", True )
                , ( "mdc-slider--focus", model.focus )
                , ( "mdc-slider--active", model.active )
                , ( "mdc-slider--in-transit", model.inTransit )
                , ( "mdc-slider--off", value <= config.min )
                ]
             , tabindex 0
             , Events.on "focus" (Json.succeed (lift Focus))
             , Events.on "blur" (Json.succeed (lift Blur))
             , dataAttr "min" (toString config.min)
             , dataAttr "max" (toString config.max)
             , dataAttr "steps" (toString config.steps)
             ]
                ++ List.map activateOn downs
             -- ++ List.map changeOn ups
             -- ++ (if model.active then
             --         List.map inputOn (List.concat [ downs, ups, moves ])
             --     else
             --         List.map inputOn downs
             --    )
            )
            [ div [ class "mdc-slider__track-container" ]
                [ div
                    [ class "mdc-slider__track"
                    , style [ ( "transform", "scaleX(" ++ toString trackScale ++ ")" ) ]
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
