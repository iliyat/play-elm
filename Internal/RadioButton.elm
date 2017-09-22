module Internal.RadioButton exposing (Msg(..))

import Internal.Ripple as Ripple


type Msg
    = RippleMsg Ripple.Msg
    | SetFocus Bool
    | NoOp
