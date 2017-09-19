module Menu
    exposing
        ( init
        , Model
        , Msg
        , view
        , onInputChange
        , defaultModel
        , update
        , onSelect
        , subscriptions
        , attach
        )

import Html exposing (Html, text, div, button, Attribute, ul, li)
import Html.Events as Events
import Html.Attributes exposing (class, classList, style)
import Json.Decode as Json
import Internal.Menu
    exposing
        ( Msg(..)
        , Geometry
        , Element
        , Common
        , element
        , decoder
        , decoderCommon
        )
import Mouse


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.open == True then
            Mouse.clicks Click
          else
            Sub.none
        ]


type Alignment
    = OpenFromTopLeft
    | OpenFromTopRight
    | OpenFromBottomLeft
    | OpenFromBottomRight



-- MODEL


type alias Model =
    { geometry : Maybe Geometry
    , open : Bool
    , top : Float
    , left : Float
    }


init : Model
init =
    { geometry = Nothing
    , open = False
    , top = 0
    , left = 0
    }


defaultModel : Model
defaultModel =
    { geometry = Nothing
    , open = False
    , top = 0
    , left = 0
    }


type alias Msg =
    Internal.Menu.Msg



-- UPDATE


update : (Msg -> m) -> Msg -> Model -> ( Model, Cmd m )
update fwd msg model =
    case msg of
        Open ->
            ( { model | open = True }, Cmd.none )

        Close ->
            ( { model | open = False }, Cmd.none )

        Click pos ->
            case model.geometry of
                Just geometry ->
                    let
                        inside { x, y } { top, left, width, height } =
                            (left <= toFloat x)
                                && (toFloat x <= left + width)
                                && (top <= toFloat y)
                                && (toFloat y <= top + height)
                    in
                        if inside pos geometry.menu.bounds then
                            ( model, Cmd.none )
                        else
                            ( { model | open = False }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        Toggle geom ->
            ( { model
                | open = True
                , geometry = Just geom
              }
            , Cmd.none
            )

        ToggleString st ->
            ( model, Cmd.none )

        Init geometry ->
            ( { model
                | geometry = Just geometry
              }
            , Cmd.none
            )



-- menu view


view : (Msg -> m) -> Model -> List (Html m) -> Html m
view lift { open, left, top } menuItems =
    let
        opacity =
            if open then
                "1"
            else
                "0"

        zIndex =
            if open then
                "2000"
            else
                "-1"

        transform =
            if open then
                "scale(1, 1)"
            else
                "scale(0, 0)"

        transform1 =
            if open then
                "scaleX(1)"
            else
                "scaleX(0)"

        transform2 =
            if open then
                "scaleY(1)"
            else
                "scaleY(0)"

        initOn event =
            Events.on event (Json.map (Init >> lift) decoder)
    in
        div
            [ classList
                [ ( "mdc-simple-menu mdc-simple-menu--open", True )
                , ( "elm-mdc-simple-menu--uninitialized", True )
                ]
            , initOn "elm-mdc-init"
            , style
                [ ( "opacity", opacity )
                , ( "transform-origin", "right top 0px" )
                , ( "transform", transform )
                , ( "transition", "transform 250ms cubic-bezier(0.23, 1, 0.32, 1) 0ms, opacity 250ms cubic-bezier(0.23, 1, 0.32, 1) 0ms" )

                -- , ( "position", "fixed" )
                -- , ( "left", toString left ++ "px" )
                -- , ( "top", toString top ++ "px" )
                , ( "z-index", zIndex )
                ]
            ]
            [ div
                [ style
                    [ ( "opacity", opacity )
                    , ( "max-height", "100%" )
                    , ( "overflow-y", "auto" )
                    , ( "transform-origin", "right top 0px" )
                    , ( "transform", transform1 )
                    , ( "transition", "transform 250ms cubic-bezier(0.23, 1, 0.32, 1) 0ms, opacity 250ms cubic-bezier(0.23, 1, 0.32, 1) 0ms" )
                    ]
                ]
                [ div
                    [ style
                        [ ( "opacity", opacity )
                        , ( "transform-origin", "right top 0px" )
                        , ( "transform", transform2 )
                        , ( "transition", "transform 500ms cubic-bezier(0.23, 1, 0.32, 1) 0ms, opacity 500ms cubic-bezier(0.23, 1, 0.32, 1) 0ms" )
                        ]
                    ]
                    [ ul [ class "mdc-simple-menu__items mdc-list" ]
                        (menuItems)
                    ]
                ]
            ]


attach : (Msg -> msg) -> Attribute msg
attach lift =
    Events.on "click" (Json.map (lift << Toggle) decoder)


onSelect : m -> Attribute m
onSelect msg =
    Events.onClick msg


onInputChange : (Common -> msg) -> Attribute msg
onInputChange msg =
    Events.on "input" (Json.map msg decoderCommon)
