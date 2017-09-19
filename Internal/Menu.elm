module Internal.Menu
    exposing
        ( Geometry
        , Element
        , Common
        , decoder
        , decoderCommon
        , element
        , Msg(..)
        )

import DOM
import Json.Decode exposing (..)
import Html.Events exposing (targetValue)
import Mouse


type Msg
    = Open
    | Close
    | Toggle Geometry
    | ToggleString String
    | Click Mouse.Position
    | Init Geometry


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


type alias Common =
    { geometry : Geometry
    , value : String
    }


{-| Decode Geometry
-}
decoder : Decoder Geometry
decoder =
    map2 Geometry
        (DOM.target element)
        (DOM.target (DOM.nextSibling element))


{-| Decode an Element
-}
element : Decoder Element
element =
    map4 Element
        DOM.offsetTop
        DOM.offsetLeft
        DOM.offsetHeight
        DOM.boundingClientRect


decoderCommon : Decoder Common
decoderCommon =
    map2 Common decoder targetValue
