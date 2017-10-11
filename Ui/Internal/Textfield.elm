module Ui.Internal.Textfield exposing (Msg(..))

import Form
import MaskedInput.Text as MaskedText


type Msg
    = Blur
    | Focus
    | Input String
    | SubmitText
    | SetValue String
    | NoOp
    | FocusChanged Bool
    | InputStateChanged MaskedText.State
