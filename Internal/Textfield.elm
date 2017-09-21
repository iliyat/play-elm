module Internal.Textfield exposing (Msg(..))


type Msg
    = Blur
    | Focus
    | Input String
    | SetValue String
    | NoOp
