module DialogDemo exposing (main)

import Date exposing (Date, Day(..), Month(..), day, dayOfWeek, month, year)
import Html exposing (Html, div, h1, text, p)
import Html.Attributes as Attr
import Task
import Date.Extra as Date
import Ui.Dialog as Dialog
import Ui.Options as Options
import Ui.Button as Button
import Ui.Ripple as Ripple


type Msg
    = CurrentDate Date
    | ButtonMsg Button.Msg
    | RippleMsg Ripple.Msg


type alias Model =
    { date : Maybe Date
    , buttonModel : Button.Model
    , ripple : Ripple.Model
    }


init : ( Model, Cmd Msg )
init =
    { date = Just <| Date.fromParts 1992 Feb 21 0 0 0 0
    , buttonModel = Button.defaultModel
    , ripple = Ripple.defaultModel
    }
        ! [ Task.perform CurrentDate Date.now ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RippleMsg msg_ ->
            let
                ( new, effects ) =
                    Ripple.update msg_ model.ripple
            in
                ( { model | ripple = new }, effects |> Cmd.map RippleMsg )

        ButtonMsg msg_ ->
            let
                ( new, effects ) =
                    Button.update msg_ model.buttonModel
            in
                ( { model | buttonModel = new }, effects |> Cmd.map ButtonMsg )

        CurrentDate today ->
            let
                initDate =
                    (Date.add Date.Day 7 today)
            in
                { model
                    | date = Just initDate
                }
                    ! []


view : Model -> Html Msg
view model =
    div [ Attr.class "mdc-typography mdc-dialog-scroll-lock" ]
        [ p [] [ text "text" ]
        , Dialog.view [ Dialog.open ]
            [ Dialog.header []
                [ Html.h2
                    [ Attr.class
                        "mdc-dialog__header__title"
                    ]
                    [ text "Подтвердить изменения" ]
                ]
            , Dialog.body []
                [ text "Заявка будет отправлена на повторную проверку.\nИзменен срок займа от одобренного. "
                ]
            , Dialog.footer []
                [ Dialog.cancelButton
                    (Button.view ButtonMsg model.buttonModel)
                    [ Button.ripple ]
                    [ text "Отмена" ]
                , Dialog.acceptButton
                    (Button.view ButtonMsg model.buttonModel)
                    [ Button.ripple ]
                    [ text "Подтвердить" ]
                ]
            ]
        , Html.node "link"
            [ Attr.rel "stylesheet"
            , Attr.href "material-components-web.css"
            ]
            []
        , Html.node "link"
            [ Attr.rel "stylesheet"
            , Attr.href "main.css"
            ]
            []
        , Html.node "link"
            [ Attr.rel "stylesheet"
            , Attr.href "https://fonts.googleapis.com/icon?family=Material+Icons"
            ]
            []
        , Html.node "link"
            [ Attr.rel "stylesheet", Attr.href "https://fonts.googleapis.com/css?family=Roboto:300,400,500" ]
            []
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
