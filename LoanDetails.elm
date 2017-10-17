module LoanDetails exposing (main, render, defaultModel, Model, Msg, update)

import Html exposing (Html, div, h1, text, p)
import Html.Attributes as Attrs exposing (style)
import Html.Events as Events
import Ui.Options as Options exposing (styled, cs, css, when)
import Ui.Elevation as Elevation
import Ui.Typography as Typography
import Icons.Icon as Icon
import Utils.Style exposing (mkClass)


type Msg
    = ToggleCard


type alias Model =
    { open : Bool
    }


defaultModel : Model
defaultModel =
    { open = False }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleCard ->
            { model | open = not model.open } ! []


view : Model -> Html Msg
view model =
    styled div
        [ cs "content-view" ]
        [ styled div
            [ Elevation.z1, cs "block" ]
            [ render model ]
        , styles
        ]


render : Model -> Html Msg
render model =
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

        summaryBlock =
            div []
                [ div [ class "block-row-header" ] [ text "Процент по займу" ]
                , div [ class "block-summary-bold" ] [ text "3052 ₽" ]
                , div [ class "block-row-header" ] [ text "В среднем в день" ]
                , div [ class "block-summary-bold" ] [ text "218 ₽" ]
                , div [ class "block-summary-bold" ] [ text "2,18%" ]
                ]
    in
        div
            []
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
                [ div [ class "table-wrapper" ]
                    [ div [ class "rows-wrapper" ]
                        [ div [ class "row" ] []
                        , div [ class "row" ] []
                        , div [ class "row" ] []
                        , div [ class "row" ] []
                        , div [ class "row" ] []
                        ]
                    , div [ class "table", Attrs.class "elm-table-wrapper" ]
                        [ block
                        , block
                        , block
                        , block
                        , block
                        , block
                        , block
                        , block
                        , block
                        , block
                        , block
                        , block
                        , block
                        , block
                        , block
                        , block
                        , block
                        , block
                        , block
                        , block
                        ]
                    , summaryBlock
                    ]
                ]
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
            , Attrs.href "styles/scroll.css"
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
    let
        init_ =
            ( defaultModel, Cmd.none )
    in
        Html.program
            { init = init_
            , update = update
            , view = view
            , subscriptions = always Sub.none
            }
