module Main exposing (..)

import Html exposing (Html, text, div, button, Attribute, ul, li)
import Html.Events exposing (on)
import Html.Attributes exposing (class, classList, style)
import Json.Decode as Json
import DOM exposing (target, offsetWidth)
import Html.Events exposing (..)
import Debug
import Mouse


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { time : String
    , geometry : Maybe Geometry
    , opened : Bool
    }


type alias Geometry =
    { button : Element
    , menu : Element
    }


type alias Element =
    { offsetTop : Float
    , offsetLeft : Float
    , offsetHeight : Float
    , bounds : DOM.Rectangle
    }


init : ( Model, Cmd Msg )
init =
    ( { time = "a", geometry = Nothing, opened = False }, Cmd.none )



-- MESSAGES


type Msg
    = NoOp
    | ButtonClick Geometry
    | Click Mouse.Position



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
                            ( { model | opened = False }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ButtonClick geom ->
            ( { model | opened = True, geometry = Just geom }, Cmd.none )

        _ ->
            Debug.log "Unknown message" ( model, Cmd.none )



-- Custom event handler


onCustomClick : (Geometry -> msg) -> Attribute msg
onCustomClick msg =
    on "click" (Json.map msg deco)


{-| Decode an Element
-}
element : Json.Decoder Element
element =
    Json.map4 Element
        DOM.offsetTop
        DOM.offsetLeft
        DOM.offsetHeight
        DOM.boundingClientRect


{-| Decode Geometry
-}
deco : Json.Decoder Geometry
deco =
    Json.map2 Geometry
        (DOM.target element)
        (DOM.target (DOM.nextSibling element))


view : Model -> Html Msg
view model =
    div []
        [ button [ onCustomClick ButtonClick, Html.Attributes.id "btn" ] [ text "click!" ]
        , menuView model.opened
        , Html.node "link"
            [ Html.Attributes.rel "stylesheet", Html.Attributes.href "https://fonts.googleapis.com/css?family=Roboto+Mono" ]
            []
        , Html.node "link"
            [ Html.Attributes.rel "stylesheet", Html.Attributes.href "https://fonts.googleapis.com/css?family=Roboto:300,400,500" ]
            []
        , Html.node "link"
            [ Html.Attributes.rel "stylesheet", Html.Attributes.href "https://unpkg.com/material-components-web@latest/dist/material-components-web.min.css" ]
            []
        ]



-- menu view


menuView : Bool -> Html.Html msg
menuView isOpen =
    div
        [ classList
            [ ( "mdc-simple-menu mdc-simple-menu--open", True )
            ]
        , style
            [ ( "width", "170" )
            , ( "opacity"
              , if isOpen then
                    "1"
                else
                    "0"
              )
            ]
        ]
        [ ul [ class "mdc-simple-menu__items mdc-list" ]
            [ li [ class "mdc-list-item" ] [ text "Back" ]
            , li [ class "mdc-list-item" ] [ text "Forward" ]
            , li [ class "mdc-list-item" ] [ text "Reload" ]
            , li [ class "mdc-list-divider" ] []
            , li [ class "mdc-list-item" ] [ text "Save As..." ]
            ]
        ]



-- SUBSCRIBTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.opened == True then
        Mouse.clicks Click
    else
        Sub.none
