module Ui.TextfieldNumber
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
import Ui.Internal.TextfieldNumber as InternalTextfield exposing (..)
import Json.Decode as Json
import Regex
import Utils.General as Utils exposing (..)
import FormatNumber exposing (format)
import Ui.Input.MaskedNumber as MaskedNumber


type alias Model =
    { isFocused : Bool
    , isDirty : Bool
    , maskedState : MaskedNumber.State
    }


defaultModel : Model
defaultModel =
    { isFocused = False
    , isDirty = False
    , maskedState = MaskedNumber.initialState
    }


type alias Msg =
    InternalTextfield.Msg


type TextfieldEvent
    = NoChange
    | Changed (Maybe Int)


externalUpdate : Msg -> Model -> Config -> Maybe Int -> ( Model, Maybe Int )
externalUpdate msg model textfieldConfig previousText =
    let
        ( newTextfieldModel, _, textfieldEvent ) =
            update
                msg
                model
                textfieldConfig
                previousText

        newText =
            case textfieldEvent of
                Changed newString ->
                    newString

                _ ->
                    previousText
    in
        ( newTextfieldModel, newText )


update : Msg -> Model -> Config -> Maybe Int -> ( Model, Cmd m, TextfieldEvent )
update msg model config previousText =
    let
        numerize =
            Regex.replace Regex.All (Regex.regex "[^0-9]") (\_ -> "")
    in
        case msg of
            Input val ->
                let
                    _ =
                        Debug.log "input" val

                    dirty =
                        case val of
                            Nothing ->
                                config.defaultValue == Nothing

                            Just int ->
                                int /= (config.defaultValue |> Maybe.withDefault 0)
                in
                    ( { model | isDirty = dirty }, Cmd.none, Changed val )

            SetValue int ->
                let
                    dirty =
                        case config.defaultValue of
                            Just a ->
                                a /= int

                            _ ->
                                True
                in
                    { model | isDirty = dirty } ! []

            Blur ->
                { model | isFocused = False } ! []

            Focus ->
                { model | isFocused = True } ! []

            InputStateChanged state ->
                let
                    _ =
                        Debug.log "InputStateChanged" state

                    val =
                        MaskedNumber.getter state previousText

                    dirty =
                        case val of
                            Nothing ->
                                config.defaultValue == Nothing

                            Just int ->
                                int /= (config.defaultValue |> Maybe.withDefault 0)
                in
                    -- ( { model | isDirty = dirty }, Cmd.none, Changed val )
                    ({ model | maskedState = state }) ! []

            FocusChanged bool ->
                model ! []

            SubmitText ->
                model ! []

            OnKeyDown key ->
                model ! []

            NoOp ->
                model ! []


type alias Config =
    { labelText : Maybe String
    , labelFloat : Bool
    , value : Maybe String
    , defaultValue : Maybe Int
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


maskedInputOptions : Config -> MaskedNumber.Options Msg
maskedInputOptions config =
    let
        defaultOptions =
            MaskedNumber.defaultOptions Input InputStateChanged
    in
        { defaultOptions
            | pattern = "(###) ###"
            , hasFocus = Just FocusChanged
        }


view : Maybe Int -> Model -> Config -> Html Msg
view value_ model config =
    let
        isFocused =
            model.isFocused && not config.disabled

        isDirty =
            model.isDirty || (config.defaultValue /= Nothing)

        value =
            value_

        asTitleStyle =
            { labelBottom =
                if model.isFocused || value /= Nothing then
                    "24px"
                else
                    "8px"
            , labelFontSize =
                if model.isFocused || value /= Nothing then
                    "16px"
                else
                    "34px"
            , height = "56px"
            , fontSize = "34px"
            }

        simpleStyle =
            { labelBottom = "8px"
            , labelFontSize = "16px"
            , height = "48px"
            , fontSize = "18px"
            }

        st =
            if config.asTitle then
                asTitleStyle
            else
                simpleStyle

        extra =
            config.extra |> Maybe.withDefault ""

        extraInside =
            Maybe.map (\e -> " " ++ e) config.extraInside |> Maybe.withDefault ""

        intValue =
            value |> Maybe.withDefault 0

        floatValue =
            intValue |> toFloat

        displayValue =
            format rusLocale (toFloat intValue)

        pl =
            Maybe.map (flip Utils.pluralize intValue) config.plural
                |> Maybe.withDefault ""

        eventOptions =
            { stopPropagation = True
            , preventDefault = True
            }

        onKeyDown tagger =
            Events.onWithOptions "keydown" eventOptions (Json.map tagger Events.keyCode)

        inputHtml =
            MaskedNumber.input
                (maskedInputOptions config)
                [ classList
                    [ ( "mdc-textfield__input", True )
                    ]
                , Events.on "change" (Json.succeed SubmitText)
                , Events.onFocus <| Focus
                , Events.onBlur <| Blur
                , Attr.value displayValue
                , style [ ( "font-size", st.fontSize ) ]
                ]
                model.maskedState
                value

        -- inputHtml =
        --     input
        --         [ Attr.type_ "text"
        --         , style [ ( "font-size", st.fontSize ) ]
        --         , classList [ ( "mdc-textfield__input", True ) ]
        --         , Events.onFocus <| Focus
        --         , Events.onBlur <| Blur
        --         , onKeyDown OnKeyDown
        --         , Events.on "change" (Json.succeed SubmitText)
        --         , Attr.value displayValue
        --         ]
        --         []
        divHtml =
            div
                [ style
                    [ ( "font-size", st.fontSize )
                    , ( "width", "168px" )
                    ]
                , classList
                    [ ( "mdc-textfield__input", True )
                    ]
                ]
                [ text <| displayValue ++ extraInside ]

        contentHtml =
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
                [ ( "height", st.height )
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
                    [ ( "bottom", st.labelBottom )
                    , ( "font-size", st.labelFontSize )
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
