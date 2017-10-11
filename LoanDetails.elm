module LoanDetails exposing (main)

import Date exposing (Date, Day(..), Month(..), day, dayOfWeek, month, year)
import Html exposing (Html, div, h1, text, p)
import Html.Attributes as Attrs exposing (style)
import Html.Events as Events
import Task
import Ui.Options as Options exposing (styled, cs, css, when)
import Date.Extra as Date
import Ui.Elevation as Elevation
import Ui.Typography as Typography
import Icons.Icon as Icon
import Utils.Style exposing (mkClass)


type Msg
    = CurrentDate Date
    | ToggleCard


type alias Model =
    { date : Maybe Date
    , open : Bool
    }


init : ( Model, Cmd Msg )
init =
    { date = Just <| Date.fromParts 1992 Feb 21 0 0 0 0
    , open = True
    }
        ! [ Task.perform CurrentDate Date.now ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleCard ->
            { model | open = not model.open } ! []

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
    let
        transformValue =
            if model.open then
                ("rotate(90deg)")
            else
                ("rotate(270deg)")

        class =
            mkClass "loan-details--"

        block =
            div [ class "block" ]
                [ div [ class "block-row-header" ] [ text "1 день" ]
                , div [ class "block-row-bold" ] [ text "10 000 ₽" ]
                , div [ class "block-row" ] []
                , div [ class "block-row" ] [ text "10 000 ₽" ]
                , div [ class "block-row" ] [ text "0 ₽" ]
                ]
    in
        styled div
            [ cs "content-view" ]
            [ styled div
                [ Elevation.z1, cs "block" ]
                [ styled div
                    [ css "display" "flex"
                    , css "justify-content" "space-between"
                    , css "align-items" "center"
                    ]
                    [ styled div
                        [ Typography.headline ]
                        [ text "Детали расчета" ]
                    , Icon.asButton "chevron_left"
                        [ Events.onClick ToggleCard
                        , style [ ( "transform", transformValue ) ]
                        ]
                    ]
                , styled div
                    [ css "padding-top" "24px", css "display" "none" |> when (not model.open) ]
                    [ div [ class "table" ]
                        [ div [ class "rows-wrapper" ]
                            [ div [ class "row" ] []
                            , div [ class "row" ] []
                            , div [ class "row" ] []
                            , div [ class "row" ] []
                            ]
                        , block
                        , block
                        ]
                    ]
                ]
            , styles
            ]


styles : Html Msg
styles =
    div []
        [ Html.node "link"
            [ Attrs.rel "stylesheet"
            , Attrs.href "material-components-web.css"
            ]
            []
        , Html.node "link"
            [ Attrs.rel "stylesheet"
            , Attrs.href "main.css"
            ]
            []
        , Html.node "link"
            [ Attrs.rel "stylesheet"
            , Attrs.href "table.css"
            ]
            []
        , Html.node "link"
            [ Attrs.rel "stylesheet"
            , Attrs.href "https://fonts.googleapis.com/icon?family=Material+Icons"
            ]
            []
        , Html.node "link"
            [ Attrs.rel "stylesheet", Attrs.href "https://fonts.googleapis.com/css?family=Roboto:300,400,500" ]
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
