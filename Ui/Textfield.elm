module Ui.Textfield
    exposing
        ( view
        , Model
        , defaultModel
        , Msg
        , update
        , Config
        , defaultConfig
        , TextfieldEvent
        , TextfieldEvent(..)
        , externalUpdate
        )

import Html exposing (Html, span, input, label, text, div, button, Attribute)
import Html.Attributes as Attr exposing (class, classList, style)
import Html.Events as Events
import Ui.Internal.Textfield as InternalTextfield exposing (..)
import Json.Decode as Json
import Regex
import Utils.General as Utils exposing (..)
import FormatNumber exposing (format)
import MaskedInput.Text as MaskedText


type alias Model =
    { isFocused : Bool
    , isDirty : Bool
    , maskedState : MaskedText.State
    }


defaultModel : Model
defaultModel =
    { isFocused = False
    , isDirty = False
    , maskedState = MaskedText.initialState
    }


type alias Msg =
    InternalTextfield.Msg


type TextfieldEvent
    = NoChange
    | Changed (Maybe String)


externalUpdate : Msg -> Model -> Config -> Maybe String -> ( Model, Maybe String )
externalUpdate msg model textfieldConfig previousText =
    let
        ( newTextfieldModel, _, textfieldEvent ) =
            update
                msg
                model
                textfieldConfig

        newText =
            case textfieldEvent of
                Changed newString ->
                    newString

                _ ->
                    previousText
    in
        ( newTextfieldModel, newText )


update : Msg -> Model -> Config -> ( Model, Cmd m, TextfieldEvent )
update msg model config =
    let
        numerize =
            Regex.replace Regex.All (Regex.regex "[^0-9]") (\_ -> "")

        numberedValue str =
            if config.numbered || config.mask /= Nothing then
                Just <| numerize str
            else
                Just str
    in
        case msg of
            Input str ->
                let
                    dirty =
                        str /= (config.defaultValue |> Maybe.withDefault "")
                in
                    ( { model | isDirty = dirty }, Cmd.none, Changed (numberedValue str) )

            SetValue str ->
                let
                    dirty =
                        case config.defaultValue of
                            Just a ->
                                a /= str

                            _ ->
                                True
                in
                    { model | isDirty = dirty } ! []

            Blur ->
                { model | isFocused = False } ! []

            Focus ->
                { model | isFocused = True } ! []

            InputStateChanged state ->
                { model | maskedState = state } ! []

            FocusChanged bool ->
                model ! []

            SubmitText ->
                model ! []

            NoOp ->
                model ! []


type alias Config =
    { labelText : Maybe String
    , labelFloat : Bool
    , value : Maybe String
    , defaultValue : Maybe String
    , disabled : Bool
    , asTitle : Bool
    , required : Bool
    , type_ : Maybe String
    , fullWidth : Bool
    , invalid : Bool
    , extra : Maybe String
    , extraInside : Maybe String
    , numbered : Bool
    , readonly : Bool
    , plural : Maybe Plural
    , mask : Maybe String
    }


defaultConfig : Config
defaultConfig =
    { labelText = Nothing
    , labelFloat = False
    , value = Nothing
    , defaultValue = Nothing
    , disabled = False
    , asTitle = False
    , required = False
    , type_ = Just "text"
    , fullWidth = False
    , invalid = False
    , extra = Nothing
    , numbered = False
    , readonly = False
    , plural = Nothing
    , extraInside = Nothing
    , mask = Nothing
    }


maskedInputOptions : Config -> MaskedText.Options Msg
maskedInputOptions config =
    let
        defaultOptions =
            MaskedText.defaultOptions Input InputStateChanged

        mask =
            config.mask |> Maybe.withDefault ""
    in
        { defaultOptions
            | pattern = mask
            , hasFocus = Just FocusChanged
        }


view : Maybe String -> Model -> Config -> Html Msg
view value_ model config =
    let
        isFocused =
            model.isFocused && not config.disabled

        isDirty =
            model.isDirty || (config.defaultValue /= Nothing)

        labelBottom =
            if config.asTitle then
                "24px"
            else
                "8px"

        height =
            if config.asTitle then
                "56px"
            else
                "48px"

        fontSize =
            if config.asTitle then
                "34px"
            else
                "18px"

        value =
            value_
                |> Maybe.withDefault
                    (config.defaultValue
                        |> Maybe.withDefault ""
                    )

        extra =
            config.extra |> Maybe.withDefault ""

        extraInside =
            Maybe.map (\e -> " " ++ e) config.extraInside |> Maybe.withDefault ""

        intValue =
            String.toInt value |> Result.withDefault 0

        floatValue =
            intValue |> toFloat

        displayValue =
            if config.numbered then
                (format rusLocale floatValue)
            else
                value

        pl =
            Maybe.map (flip Utils.pluralize intValue) config.plural
                |> Maybe.withDefault ""

        maskedInputHtml =
            MaskedText.input
                (maskedInputOptions config)
                [ classList
                    [ ( "mdc-textfield__input", True )
                    ]
                , Events.on "change" (Json.succeed SubmitText)
                , Events.onFocus <| Focus
                , Events.onBlur <| Blur
                ]
                model.maskedState
                value

        inputHtml =
            input
                [ Attr.type_ "text"
                , style [ ( "font-size", fontSize ) ]
                , classList [ ( "mdc-textfield__input", True ) ]
                , Events.onFocus <| Focus
                , Events.onBlur <| Blur
                , Events.onInput Input
                , Events.on "change" (Json.succeed SubmitText)
                , Attr.value displayValue
                ]
                []

        divHtml =
            div
                [ style
                    [ ( "font-size", fontSize )
                    , ( "width", "168px" )
                    ]
                , classList
                    [ ( "mdc-textfield__input", True )
                    ]
                ]
                [ text <| displayValue ++ extraInside ]

        contentHtml =
            if config.readonly then
                divHtml
            else if config.mask /= Nothing then
                maskedInputHtml
            else
                inputHtml
    in
        div
            [ classList
                [ ( "mdc-textfield mdc-textfield--upgraded", True )
                , ( "mdc-textfield--focused", isFocused )
                , ( "mdc-textfield--disabled", config.disabled )
                , ( "ui-textfield--readonly", config.readonly )
                , ( "mdc-textfield--fullwidth", False )
                , ( "mdc-textfield--invalid", False )
                ]
            , Events.onFocus <| Focus
            , Events.onBlur <| Blur
            , style
                [ ( "height", height )
                , ( "width"
                  , if config.fullWidth then
                        "100%"
                    else
                        "initial"
                  )
                ]
            ]
            [ contentHtml
            , label
                [ classList
                    [ ( "mdc-textfield__label mdc-typography", True )
                    , ( "mdc-textfield__label--float-above", isFocused || isDirty )
                    ]
                , style
                    [ ( "bottom", labelBottom )
                    ]
                ]
                (case config.labelText of
                    Just str ->
                        [ text str ]

                    Nothing ->
                        []
                )
            , span
                [ style
                    [ ( "float", "right" )
                    , ( "position", "absolute" )
                    , ( "right", "0" )
                    , ( "bottom", "10px" )
                    , ( "height", "24px" )
                    , ( "font-family", "Roboto" )
                    , ( "font-size", "34px" )
                    , ( "line-height", "15px" )
                    , ( "color", "rgba(0, 0, 0, 0.38)" )
                    ]
                ]
                [ text <| extra ++ pl ]
            , div [ class "mdc-textfield__bottom-line" ] []
            ]


(!) : Model -> List (Cmd m) -> ( Model, Cmd m, TextfieldEvent )
(!) m cs =
    ( m, Cmd.batch cs, NoChange )
