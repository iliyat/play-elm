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
import Select
import Views.Stepper exposing (step, stepLine)
import Ui.Button as Button
import Step.PassportCheck as PassportCheck
import Step.Conditions as Conditions
import Step.ReceiveType
import Step.PrintDocuments as PrintDocuments
import Step.IssueLoan


type Step
    = PassportCheck PassportCheck.Model
    | Conditions Conditions.Model
    | ReceiveType
    | PrintDocuments PrintDocuments.Model
    | IssueLoan


type Msg
    = CurrentDate Date
    | OnNextClick
    | OnPrevClick
    | OnClientDeclineClick
    | PrevRipple Button.Msg
    | NextRipple Button.Msg
    | SelectMsg Select.Msg
    | NoOp
    | ClientDeclineRipple Button.Msg
    | ClientDeclineDialogRipple Button.Msg
    | ClientDeclineAccept
    | ConditionsMsg Conditions.Msg
    | PassportCheckMsg PassportCheck.Msg
    | PrintDocumentsMsg PrintDocuments.Msg


nextStep : Step -> Step
nextStep current =
    case current of
        PassportCheck _ ->
            let
                ( model, effects ) =
                    Conditions.init
            in
                Conditions model

        Conditions _ ->
            PrintDocuments PrintDocuments.defaultModel

        ReceiveType ->
            PrintDocuments PrintDocuments.defaultModel

        PrintDocuments _ ->
            IssueLoan

        IssueLoan ->
            IssueLoan


prevStep : Step -> Step
prevStep current =
    let
        ( conditionModel, conditionEffects ) =
            Conditions.init
    in
        case current of
            PassportCheck _ ->
                PassportCheck PassportCheck.defaultModel

            Conditions _ ->
                PassportCheck PassportCheck.defaultModel

            ReceiveType ->
                Conditions conditionModel

            PrintDocuments _ ->
                Conditions conditionModel

            IssueLoan ->
                PrintDocuments PrintDocuments.defaultModel


type alias Model =
    { date : Maybe Date
    , currentStep : Step
    , clientDeclineDialogOpen : Bool
    , clientDeclineButtonModel : Button.Model
    , clientDeclineButtonDialogModel : Button.Model
    , clientDeclineSelected : Maybe String
    , prevButtonModel : Button.Model
    , selectModel : Select.Model
    , nextButtonModel : Button.Model
    }


init : ( Model, Cmd Msg )
init =
    let
        ( condInitModel, _ ) =
            Conditions.init
    in
        { date = Just <| Date.fromParts 1992 Feb 21 0 0 0 0
        , currentStep = PassportCheck PassportCheck.defaultModel
        , clientDeclineButtonModel = Button.defaultModel
        , prevButtonModel = Button.defaultModel
        , nextButtonModel = Button.defaultModel
        , clientDeclineButtonDialogModel = Button.defaultModel
        , clientDeclineDialogOpen = False
        , clientDeclineSelected = Just "something"
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

        OnClientDeclineClick ->
            ({ model | clientDeclineDialogOpen = True }) ! []

        ClientDeclineAccept ->
            ({ model | clientDeclineDialogOpen = False }) ! []

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

        ClientDeclineRipple msg_ ->
            let
                ( new, effects ) =
                    Button.update msg_ model.clientDeclineButtonModel
            in
                ( { model | clientDeclineButtonModel = new }
                , effects |> Cmd.map ClientDeclineRipple
                )

        ClientDeclineDialogRipple msg_ ->
            let
                ( new, effects ) =
                    Button.update msg_ model.clientDeclineButtonDialogModel
            in
                ( { model | clientDeclineButtonDialogModel = new }
                , effects |> Cmd.map ClientDeclineDialogRipple
                )

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

        ConditionsMsg msg_ ->
            let
                ( condInitModel, _ ) =
                    Conditions.init

                getModel =
                    case model.currentStep of
                        Conditions a ->
                            a

                        _ ->
                            condInitModel

                ( new, effects ) =
                    Conditions.update msg_ getModel
            in
                ( { model | currentStep = Conditions new }
                , effects |> Cmd.map ConditionsMsg
                )

        PassportCheckMsg msg_ ->
            let
                getModel =
                    case model.currentStep of
                        PassportCheck a ->
                            a

                        _ ->
                            PassportCheck.defaultModel

                ( new, effects ) =
                    PassportCheck.update msg_ getModel
            in
                ( { model | currentStep = PassportCheck new }
                , effects |> Cmd.map PassportCheckMsg
                )

        PrintDocumentsMsg msg_ ->
            let
                getModel =
                    case model.currentStep of
                        PrintDocuments a ->
                            a

                        _ ->
                            PrintDocuments.defaultModel

                ( new, effects ) =
                    PrintDocuments.update msg_ getModel
            in
                ( { model | currentStep = PrintDocuments new }, effects |> Cmd.map PrintDocumentsMsg )


getCurrentView : Model -> Html Msg
getCurrentView model =
    case model.currentStep of
        PassportCheck subModel ->
            PassportCheck.view subModel |> Html.map PassportCheckMsg

        Conditions model ->
            Conditions.view model |> Html.map (ConditionsMsg)

        ReceiveType ->
            Step.ReceiveType.view |> Html.map never

        PrintDocuments subModel ->
            PrintDocuments.view subModel |> Html.map PrintDocumentsMsg

        IssueLoan ->
            Step.IssueLoan.view |> Html.map never


stepper : Model -> Html Msg
stepper model =
    let
        class =
            mkClass "stepper--"

        classList =
            mkClassList "stepper--"

        ifCurrent int =
            case model.currentStep of
                PassportCheck _ ->
                    1 == int

                Conditions _ ->
                    2 == int

                ReceiveType ->
                    3 == int

                PrintDocuments _ ->
                    4 == int

                IssueLoan ->
                    5 == int

        tfConfig =
            Textfield.defaultConfig

        selectOptions =
            [ "Передумал", "Не ответил", "Заснул" ]

        ( condInitModel, _ ) =
            Conditions.init
    in
        div
            []
            [ styled Html.div
                [ Elevation.z1 ]
                [ div [ class "header" ]
                    [ step "Проверка паспорта" 1 (ifCurrent 1)
                    , stepLine
                    , step "Условия займа" 2 (ifCurrent 2)
                    , stepLine
                    , step "Способ получения" 3 (ifCurrent 3)
                    , stepLine
                    , step "Печать документов" 4 (ifCurrent 4)
                    , stepLine
                    , step "Выдача денежных средств" 5 (ifCurrent 5)
                    ]
                ]
            , getCurrentView model
            , div
                [ class "actions" ]
                [ div []
                    [ Button.view ClientDeclineRipple
                        model.clientDeclineButtonModel
                        [ Button.ripple
                        , Button.danger
                        , Options.onClick OnClientDeclineClick
                        ]
                        [ text "Отказ клиента" ]
                    ]
                , div []
                    [ Button.view PrevRipple
                        model.prevButtonModel
                        [ Button.ripple, Options.onClick OnPrevClick ]
                        [ text "Назад" ]
                    , Button.view NextRipple
                        model.nextButtonModel
                        [ Button.ripple
                        , Button.raised
                        , Button.primary
                        , css "margin-left" "8px"
                        , Options.onClick OnNextClick
                        ]
                        [ text "Дальше" ]
                    ]
                ]
            , Dialog.view
                [ Dialog.open
                    |> Options.when model.clientDeclineDialogOpen
                ]
                [ Dialog.header []
                    [ Html.h2
                        [ Attrs.class "mdc-dialog__header__title"
                        ]
                        [ text "Отказ клиента"
                        ]
                    ]
                , Dialog.body []
                    [ Select.view
                        model.selectModel
                        (Select.config "Причина отказа" model.clientDeclineSelected selectOptions)
                        |> Html.map SelectMsg
                    ]
                , Dialog.footer []
                    [ Dialog.acceptButton
                        (Button.view ClientDeclineDialogRipple
                            model.clientDeclineButtonDialogModel
                        )
                        [ Button.ripple
                        , Button.raised
                        , Button.primary
                        , Options.onClick ClientDeclineAccept
                        ]
                        [ text "Отправить" ]
                    ]
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
                                [ Attrs.style [ ( "display", "flex" ) ] ]
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

                -- , Attrs.href "material-components-web.css"
                , Attrs.href "mdc-webpack.css"
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
                , Attrs.href
                    "datepicker.css"
                ]
                []
            ]


mkClass : String -> String -> Html.Attribute msg
mkClass classNamespace c =
    Attrs.class (classNamespace ++ c)


mkClassList : String -> List ( String, Bool ) -> Html.Attribute msg
mkClassList classNamespace cs =
    List.map (\( c, b ) -> ( classNamespace ++ c, b )) cs
        |> Attrs.classList


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
