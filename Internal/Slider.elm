module Internal.Slider exposing (Msg(..), Geometry, defaultGeometry)

import Mouse


type Msg m
    = NoOp
    | Focus
    | Blur
    | Input Float
    | Activate Bool Geometry
    | Drag Geometry
    | SetValue Float
    | Up
    | Tick
    | Dispatch (List m)
    | Init Geometry
    | Resize
    | AnimationFrame
    | MouseUp Mouse.Position
    | MouseDrag Mouse.Position


type alias Geometry =
    { width : Float
    , left : Float
    , x : Float
    , discrete : Bool
    , steps : Int
    , min : Float
    , max : Float
    }


defaultGeometry : Geometry
defaultGeometry =
    { width = 0
    , left = 0
    , x = 0
    , discrete = True
    , min = 0
    , max = 100
    , steps = 1
    }
