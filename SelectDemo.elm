module SelectDemo exposing (main)

import Date exposing (Date, Day(..), Month(..), day, dayOfWeek, month, year)
import Html exposing (Html, div, h1, text, p)
import Html.Attributes as Attr
import Task
import Date.Extra as Date
import Ui.Button as Button
import Ui.Ripple as Ripple
import Select


type Msg
    = CurrentDate Date
    | SelectMsg Select.Msg


type alias Model =
    { date : Maybe Date
    , buttonModel : Button.Model
    , selectModel : Select.Model
    }


init : ( Model, Cmd Msg )
init =
    { date = Just <| Date.fromParts 1992 Feb 21 0 0 0 0
    , buttonModel = Button.defaultModel
    , selectModel = Select.defaultModel
    }
        ! [ Task.perform CurrentDate Date.now ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectMsg msg_ ->
            let
                ( newModel, _ ) =
                    Select.update
                        msg_
                        model.selectModel
            in
                ( { model | selectModel = newModel }, Cmd.none )

        CurrentDate today ->
            let
                initDate =
                    (Date.add Date.Day 7 today)
            in
                { model
                    | date = Just initDate
                }
                    ! []


selectConfig : Select.Config
selectConfig =
    Select.Config [ "one", "two" ]


view : Model -> Html Msg
view model =
    div []
        [ p [] [ text "text" ]
        , Select.view model.selectModel selectConfig |> Html.map SelectMsg
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map SelectMsg (Select.subscriptions model.selectModel)
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
