module Internal.Textfield exposing (Msg(..))


type Msg
    = Blur
    | Focus
    | Input String
    | SubmitText
    | SetValue String
    | NoOp
