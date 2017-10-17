module StepperDemo exposing (main)

import Html exposing (Html, div, h1, text, p, span)
import Html.Attributes as Attrs
import Ui.Options as Options exposing (styled, cs, css)
import Ui.Elevation as Elevation
import Ui.Typography as Typography
import Ui.Button as Button
import Svg
import Svg.Attributes as Svg


type Step
    = PassportCheck
    | Conditions
    | ReceiveType
    | PrintDocuments
    | IssueLoan


type Msg
    = NoOp
    | ChangeStep Step
    | OnNextClick
    | OnPrevClick
    | PrevRipple Button.Msg
    | NextRipple Button.Msg


nextStep : Step -> Step
nextStep current =
    case current of
        PassportCheck ->
            Conditions

        Conditions ->
            ReceiveType

        ReceiveType ->
            PrintDocuments

        PrintDocuments ->
            IssueLoan

        IssueLoan ->
            IssueLoan


prevStep : Step -> Step
prevStep current =
    case current of
        PassportCheck ->
            PassportCheck

        Conditions ->
            PassportCheck

        ReceiveType ->
            Conditions

        PrintDocuments ->
            ReceiveType

        IssueLoan ->
            PrintDocuments


type alias Model =
    { currentStep : Step
    , prevButtonModel : Button.Model
    , nextButtonModel : Button.Model
    }


init : ( Model, Cmd Msg )
init =
    { currentStep = PassportCheck
    , prevButtonModel = Button.defaultModel
    , nextButtonModel = Button.defaultModel
    }
        ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnPrevClick ->
            let
                current =
                    prevStep model.currentStep
            in
                { model | currentStep = current } ! []

        OnNextClick ->
            let
                current =
                    nextStep model.currentStep
            in
                { model | currentStep = current } ! []

        PrevRipple msg_ ->
            let
                ( new, effects ) =
                    Button.update msg_ model.prevButtonModel
            in
                ( { model | prevButtonModel = new }
                , effects |> Cmd.map PrevRipple
                )

        NextRipple msg_ ->
            let
                ( new, effects ) =
                    Button.update msg_ model.nextButtonModel
            in
                ( { model | nextButtonModel = new }
                , effects |> Cmd.map NextRipple
                )

        ChangeStep step ->
            case step of
                PassportCheck ->
                    model ! []

                _ ->
                    model ! []

        NoOp ->
            model ! []


step : String -> Int -> Bool -> Html Msg
step title num disabled =
    let
        class =
            mkClass "stepper--"

        classList =
            mkClassList "stepper--"

        circleColor =
            if disabled then
                "rgb(158, 158, 158)"
            else
                "#009ce1"
    in
        div
            [ classList
                [ ( "step", True ), ( "step--disabled", disabled ) ]
            ]
            [ span [ class "circle-wrap" ]
                [ span [ class "circle-pad" ]
                    [ Svg.svg
                        [ Svg.class "ci"
                        , Svg.width "21"
                        , Svg.height "21"
                        ]
                        [ Svg.circle
                            [ Svg.cx "12"
                            , Svg.cy "12"
                            , Svg.r "10"
                            , Svg.fill circleColor
                            ]
                            []
                        , Svg.text_
                            [ Svg.x "12"
                            , Svg.y "16"
                            , Svg.fontSize "12"
                            , Svg.fill "#fff"
                            ]
                            [ Svg.text <| toString num ]
                        ]
                    ]
                ]
            , div [ class "text" ]
                [ text title
                ]
            ]


stepLine : Html Msg
stepLine =
    let
        class =
            mkClass "stepper--"
    in
        div [ class "step-line" ]
            [ span [] []
            ]


passportCheckView : Html Msg
passportCheckView =
    div []
        [ styled div [ Typography.headline ] [ text "Проверка паспорта" ]
        ]


view : Model -> Html Msg
view model =
    let
        class =
            mkClass "stepper--"

        classList =
            mkClassList "stepper--"

        ifCurrent step =
            if model.currentStep /= step then
                True
            else
                False
    in
        styled div
            [ cs "content-view" ]
            [ styled Html.div
                [ Elevation.z1 ]
                [ div [ class "header" ]
                    [ step "Проверка паспорта" 1 (ifCurrent PassportCheck)
                    , stepLine
                    , step "Условия займа" 2 (ifCurrent Conditions)
                    , stepLine
                    , step "Способ получения" 3 (ifCurrent ReceiveType)
                    , stepLine
                    , step "Печать документов" 4 (ifCurrent PrintDocuments)
                    , stepLine
                    , step "Выдача денежных средств" 5 (ifCurrent IssueLoan)
                    ]
                , styled Html.div
                    [ cs "block" ]
                    [ styled div [ Typography.headline ] [ text "Проверка паспорта" ]
                    ]
                ]
            , div
                [ class "actions" ]
                [ Button.view NextRipple
                    model.nextButtonModel
                    [ Button.ripple
                    , Button.raised
                    , Button.secondary
                    , css "margin-left" "8px"
                    , Options.onClick OnNextClick
                    ]
                    [ text "Дальше" ]
                , Button.view PrevRipple
                    model.prevButtonModel
                    [ Button.ripple, Options.onClick OnPrevClick ]
                    [ text "Назад" ]
                ]
            , Html.node "link"
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
                , Attrs.href "stepper.css"
                ]
                []
            , Html.node "link"
                [ Attrs.rel "stylesheet"
                , Attrs.href "https://fonts.googleapis.com/icon?family=Material+Icons"
                ]
                []
            , Html.node "link"
                [ Attrs.rel "stylesheet", Attrs.href "https://fonts.googleapis.com/css?family=Roboto:400,300,500|Roboto+Mono|Roboto+Condensed:400,700&subset=latin,latin-ext" ]
                []
            , Html.node "link"
                [ Attrs.rel "stylesheet", Attrs.href "https://cdnjs.cloudflare.com/ajax/libs/material-design-iconic-font/2.2.0/css/material-design-iconic-font.css" ]
                []
            ]


mkClass : String -> String -> Html.Attribute msg
mkClass classNamespace c =
    Attrs.class (classNamespace ++ c)


mkClassList : String -> List ( String, Bool ) -> Html.Attribute msg
mkClassList classNamespace cs =
    List.map (\( c, b ) -> ( classNamespace ++ c, b )) cs
        |> Attrs.classList


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
