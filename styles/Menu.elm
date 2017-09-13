module Menu
    exposing
        ( init
        , Model
        , Geometry
        , Common
        , Element
        , menuView
        , decoder
        , subscriptions
        , mouseClick
        , updateModel
        , onInputChange
        )

import Html exposing (Html, text, div, button, Attribute, ul, li)
import Html.Events exposing (on, targetValue)
import Html.Attributes exposing (class, classList, style)
import Json.Decode as Json
import DOM exposing (target, offsetWidth)
import Html.Events exposing (..)
import Mouse


-- MODEL


type alias Model =
    { geometry : Maybe Geometry
    , open : Bool
    , top : Float
    , left : Float
    }


type alias Geometry =
    { button : Element
    , menu : Element
    }


type alias Common =
    { geometry : Geometry
    , value : String
    }


type alias Element =
    { offsetTop : Float
    , offsetLeft : Float
    , offsetHeight : Float
    , bounds : DOM.Rectangle
    }


init : Model
init =
    { geometry = Nothing
    , open = False
    , top = 0
    , left = 0
    }


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


decoderCommon : Json.Decoder Common
decoderCommon =
    Json.map2 Common decoder targetValue



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


onInputChange : (Common -> msg) -> Attribute msg
onInputChange msg =
    on "input" (Json.map msg decoderCommon)


onMenuClick : (Geometry -> msg) -> Attribute msg
onMenuClick msg =
    onWithOptions "click" { stopPropagation = True, preventDefault = True } (Json.map msg decoder)


updateModel : Geometry -> Model
updateModel geom =
    { open = True
    , top = geom.button.bounds.top
    , left = geom.button.bounds.left - 170 + geom.button.bounds.width
    , geometry = Just geom
    }


mouseClick : Mouse.Position -> Model -> Geometry -> Model
mouseClick pos model geometry =
    let
        inside { x, y } { top, left, width, height } =
            (left <= toFloat x)
                && (toFloat x <= left + width)
                && (top <= toFloat y)
                && (toFloat y <= top + height)

        os =
            Debug.log "pos" <| pos

        _ =
            Debug.log "inside" <| inside pos geometry.menu.bounds
    in
        if inside pos geometry.menu.bounds then
            model
        else
            { model
                | open = False
                , top = geometry.button.bounds.top
                , left = geometry.button.bounds.left - 170 + geometry.button.bounds.width
            }


{-| Component subscriptions.
-}
subscriptions : Model -> (Mouse.Position -> msg) -> Sub msg
subscriptions model msg =
    Sub.batch
        [ if model.open == True then
            Mouse.clicks msg
          else
            Sub.none
        ]
