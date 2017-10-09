module IssueLoan exposing (main)

import Date exposing (Date, Day(..), Month(..), day, dayOfWeek, month, year)
import Html exposing (Html, div, h1, text, p, span)
import Html.Attributes as Attrs
import Task
import Date.Extra as Date
import Ui.Dialog as Dialog
import Ui.Textfield as Textfield
import Ui.Options as Options exposing (styled, cs, css)
import Ui.Elevation as Elevation
import Ui.Button as Button
import Views.Stepper exposing (step, stepLine)
import Ui.Button as Button
import Step.PassportCheck
import Step.Conditions


type Step
    = PassportCheck
    | Conditions
    | ReceiveType
    | PrintDocuments
    | IssueLoan


type Msg
    = CurrentDate Date
    | ChangeStep Step
    | OnNextClick
    | OnPrevClick
    | PrevRipple Button.Msg
    | NextRipple Button.Msg
    | NoOp


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
    { date : Maybe Date
    , currentStep : Step
    , prevButtonModel : Button.Model
    , nextButtonModel : Button.Model
    }


init : ( Model, Cmd Msg )
init =
    { date = Just <| Date.fromParts 1992 Feb 21 0 0 0 0
    , currentStep = PassportCheck
    , prevButtonModel = Button.defaultModel
    , nextButtonModel = Button.defaultModel
    }
        ! [ Task.perform CurrentDate Date.now ]


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

        CurrentDate today ->
            let
                initDate =
                    (Date.add Date.Day 7 today)
            in
                { model
                    | date = Just initDate
                }
                    ! []


getCurrentView : Step -> Html Msg
getCurrentView step =
    case step of
        PassportCheck ->
            Step.PassportCheck.view |> Html.map never

        Conditions ->
            Step.Conditions.view |> Html.map never

        _ ->
            div [] [ text "no view" ]


stepper : Model -> Html Msg
stepper model =
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
        div
            []
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
                , getCurrentView model.currentStep
                ]
            , div
                [ class "actions" ]
                [ Button.view NextRipple
                    model.nextButtonModel
                    [ Button.ripple
                    , Button.raised
                    , Button.primary
                    , css "margin-left" "8px"
                    , Options.onClick OnNextClick
                    ]
                    [ text "Дальше" ]
                , Button.view PrevRipple
                    model.prevButtonModel
                    [ Button.ripple, Options.onClick OnPrevClick ]
                    [ text "Назад" ]
                ]
            ]


view : Model -> Html Msg
view model =
    let
        tfConfig =
            Textfield.defaultConfig

        noOp _ =
            NoOp

        class =
            mkClass "issue-loan--"

        classList =
            mkClassList "issue-loan--"

        nameConfig =
            { tfConfig
                | labelText = Just "ФИО"
                , asTitle = True
                , fullWidth = True
                , readonly =
                    True
            }

        dateOfBirthConfig =
            { tfConfig | labelText = Just "Дата рождения", readonly = True }

        passportConfig =
            { tfConfig | labelText = Just "Паспорт", readonly = True }

        phoneConfig =
            { tfConfig | labelText = Just "Моб. телефон", readonly = True }

        loanNumberConfig =
            { tfConfig | labelText = Just "Номер займа", readonly = True }

        avatarUrl =
            "url(https://scontent-sea1-1.cdninstagram.com/t51.2885-15/s480x480/e15/11370968_1040105556019723_460503715_n.jpg?ig_cache_key=MTA3Nzg2MzAzMzEyOTIyMDUyMA%3D%3D.2)"
    in
        div [ Attrs.class "content-view" ]
            [ styled Html.div
                [ Elevation.z1 ]
                [ div [ class "base-info" ]
                    [ div [ Attrs.class "ui-flex" ]
                        [ div [ class "avatar" ]
                            [ div
                                [ Attrs.style [ ( "background-image", avatarUrl ) ] ]
                                []
                            ]
                        , div []
                            [ Textfield.view (Just "Казаков Константин Константинопольский")
                                Textfield.defaultModel
                                nameConfig
                                |> Html.map noOp
                            , div
                                []
                                [ Textfield.view (Just "24.05.1990")
                                    Textfield.defaultModel
                                    dateOfBirthConfig
                                    |> Html.map noOp
                                , Textfield.view (Just "4009 122145")
                                    Textfield.defaultModel
                                    passportConfig
                                    |> Html.map noOp
                                , Textfield.view (Just "+7 (921) 334-34-12")
                                    Textfield.defaultModel
                                    phoneConfig
                                    |> Html.map noOp
                                , Textfield.view (Just "435-12532")
                                    Textfield.defaultModel
                                    loanNumberConfig
                                    |> Html.map noOp
                                ]
                            ]
                        ]
                    ]
                ]
            , stepper model
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
                , Attrs.href "issue-loan.css"
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