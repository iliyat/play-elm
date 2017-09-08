module MenuUi exposing (Geometry, menuView, decoder)

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
    { geometry : Maybe Geometry
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
    ( { geometry = Nothing
      , opened = False
      }
    , Cmd.none
    )



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
                            ( { model
                                | opened = False
                              }
                            , Cmd.none
                            )

                Nothing ->
                    ( model, Cmd.none )

        ButtonClick geom ->
            ( { model
                | opened = True
                , geometry = Just geom
              }
            , Cmd.none
            )

        _ ->
            Debug.log "Unknown message" ( model, Cmd.none )



-- Custom event handler


onMenuClick : (Geometry -> msg) -> Attribute msg
onMenuClick msg =
    on "click" (Json.map msg decoder)


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
decoder : Json.Decoder Geometry
decoder =
    Json.map2 Geometry
        (DOM.target element)
        (DOM.target (DOM.nextSibling element))


view : Model -> Html Msg
view model =
    div [ style [ ( "width", "400px" ), ( "text-align", "right" ) ] ]
        [ button [ onMenuClick ButtonClick, Html.Attributes.id "btn" ] [ text "click!" ]
        , menuView model.opened 0 0
        , Html.node "link"
            [ Html.Attributes.rel "stylesheet"
            , Html.Attributes.href "style.css?v.10.1"
            ]
            []
        , Html.node "link"
            [ Html.Attributes.rel "stylesheet", Html.Attributes.href "https://fonts.googleapis.com/css?family=Roboto:300,400,500" ]
            []
        , Html.node "link"
            [ Html.Attributes.rel "stylesheet", Html.Attributes.href "https://unpkg.com/material-components-web@latest/dist/material-components-web.min.css" ]
            []
        ]



-- menu view


menuView : Bool -> Float -> Float -> Html.Html msg
menuView isOpen top left =
    let
        opacity =
            if isOpen then
                "1"
            else
                "0"

        zIndex =
            if isOpen then
                "2000"
            else
                "-1"

        transform =
            if isOpen then
                "scale(1, 1)"
            else
                "scale(0, 0)"

        transform1 =
            if isOpen then
                "scaleX(1)"
            else
                "scaleX(0)"

        transform2 =
            if isOpen then
                "scaleY(1)"
            else
                "scaleY(0)"
    in
        div
            [ class "mdc-simple-menu mdc-simple-menu--open"
            , style
                [ ( "opacity", opacity )
                , ( "transform-origin", "right top 0px" )
                , ( "transform", transform )
                , ( "transition", "transform 250ms cubic-bezier(0.23, 1, 0.32, 1) 0ms, opacity 250ms cubic-bezier(0.23, 1, 0.32, 1) 0ms" )
                , ( "position", "fixed" )
                , ( "left", toString left ++ "px" )
                , ( "top", toString top ++ "px" )
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
                        [ li [ class "mdc-list-item" ] [ text "Редактировать" ]
                        , li [ class "mdc-list-item" ] [ text "Отправить в архив" ]
                        , li [ class "mdc-list-divider" ] []
                        , li [ class "mdc-list-item" ] [ text "Создать дубликат" ]
                        ]
                    ]
                ]
            ]



-- SUBSCRIBTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.opened == True then
            Mouse.clicks Click
          else
            Sub.none
        ]
