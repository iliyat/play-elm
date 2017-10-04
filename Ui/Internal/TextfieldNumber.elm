module Ui.Internal.TextfieldNumber exposing (Msg(..))

import Ui.Input.MaskedNumber as MaskedNumber


type Msg
    = Blur
    | Focus
    | Input (Maybe Int)
    | OnKeyDown Int
    | SubmitText
    | SetValue Int
    | NoOp
    | FocusChanged Bool
    | InputStateChanged MaskedNumber.State
