module Ui.Textfield
    exposing
        ( view
        , viewReadonly
        , Model
        , defaultModel
        , Msg
        , update
        , Config
        , defaultConfig
        , TextfieldEvent
        , TextfieldEvent(..)
        , externalUpdate
        , render
        )

import Html exposing (Html, span, input, label, text, div, button, Attribute, p)
import Html.Attributes as Attr exposing (class, classList, style, tabindex)
import Html.Events as Events
import Ui.Internal.Textfield as InternalTextfield
    exposing
        ( Msg(..)
        , Geometry
        , geometryDecoder
        )
import Json.Decode as Json
import Regex
import Utils.General as Utils exposing (..)
import FormatNumber exposing (format)
import MaskedInput.Text as MaskedText


type alias Model =
    { isFocused : Bool
    , isDirty : Bool
    , maskedState : MaskedText.State
    , geometry : Maybe Geometry
    }


defaultModel : Model
defaultModel =
    { isFocused = False
    , isDirty = False
    , maskedState = MaskedText.initialState
    , geometry = Nothing
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
            if (config.numbered || config.mask /= Nothing) then
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

            InputClick geom ->
                { model
                    | geometry = Just geom
                }
                    ! []

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
    , errorText : String
    , formName : Maybe String
    , tabindex : Int
    , width : Int
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
    , errorText = ""
    , formName = Nothing
    , tabindex = -1
    , width = 168
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


getWidth : Config -> String
getWidth config =
    if config.fullWidth then
        "100%"
    else
        toString config.width |> flip (++) "px"


viewReadonly : Maybe String -> Model -> Config -> Html Never
viewReadonly value_ model config =
    let
        isFocused =
            model.isFocused && not config.disabled

        value =
            value_
                |> Maybe.withDefault
                    (config.defaultValue
                        |> Maybe.withDefault ""
                    )

        asTitleStyle =
            { labelBottom = "24px"
            , labelFontSize = "16px"
            , height = "56px"
            , fontSize = "34px"
            }

        simpleStyle =
            { labelBottom = "8px"
            , labelFontSize = "16px"
            , height = "48px"
            , fontSize = "16px"
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
            String.toInt value |> Result.withDefault 0

        floatValue =
            intValue |> toFloat

        displayValue =
            value

        pl =
            Maybe.map (flip Utils.pluralize intValue) config.plural
                |> Maybe.withDefault ""

        divHtml =
            div
                [ style
                    [ ( "font-size", st.fontSize )
                    , ( "width", getWidth config )
                    ]
                , classList
                    [ ( "mdc-textfield__input", True )
                    ]
                ]
                [ text <| displayValue ++ extraInside ]

        contentHtml =
            divHtml
    in
        div []
            [ div
                [ classList
                    [ ( "mdc-textfield mdc-textfield--upgraded", True )
                    , ( "mdc-textfield--focused", isFocused )
                    , ( "mdc-textfield--disabled", config.disabled )
                    , ( "ui-textfield--readonly", config.readonly )
                    , ( "mdc-textfield--fullwidth", False )
                    , ( "mdc-textfield--invalid", config.invalid )
                    ]
                , style
                    [ ( "height", st.height )
                    , ( "width", getWidth config )
                    ]
                ]
                [ contentHtml
                , label
                    [ classList
                        [ ( "mdc-textfield__label mdc-typography", True )
                        , ( "mdc-textfield__label--float-above", True )
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
            ]


render : Model -> Config -> Html Msg
render model config =
    view config.value model config


infixr 5 :>
(:>) : (a -> b) -> a -> b
(:>) f x =
    f x


view : Maybe String -> Model -> Config -> Html Msg
view value_ model config =
    let
        isFocused =
            model.isFocused && not config.disabled

        isDirty =
            model.isDirty
                || (config.defaultValue /= Nothing)
                || value_
                /= Nothing

        value =
            value_
                |> Maybe.withDefault
                    (config.value
                        |> Maybe.withDefault
                            (config.defaultValue
                                |> Maybe.withDefault ""
                            )
                    )

        asTitleStyle =
            { labelBottom =
                if model.isFocused || value /= "" then
                    "24px"
                else
                    "8px"
            , labelFontSize =
                if model.isFocused || value /= "" then
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
            , fontSize = "16px"
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

        errorClasses =
            [ ( "mdc-textfield-helptext mdc-textfield-helptext--validation-msg", True )
            , ( "mdc-textfield-helptext--persistent", True )
            ]

        maskedInputHtml =
            MaskedText.input
                (maskedInputOptions config)
                [ classList
                    [ ( "mdc-textfield__input maskedInputHtml", True )
                    ]
                , Events.on "change" (Json.succeed SubmitText)
                , Events.on "click" (Json.map InputClick geometryDecoder)
                , Events.on "focus" (Json.succeed (Focus))
                , Events.onBlur <| Blur
                , style [ ( "font-size", st.fontSize ) ]
                , tabindex config.tabindex
                ]
                model.maskedState
                value

        inputHtml =
            input
                [ Attr.type_ "text"
                , style [ ( "font-size", st.fontSize ) ]
                , classList [ ( "mdc-textfield__input", True ) ]
                , Events.on "focus" (Json.succeed Focus)
                , Events.onBlur <| Blur
                , Events.onInput Input
                , Events.on "change" (Json.succeed SubmitText)
                , Attr.value displayValue
                , tabindex config.tabindex
                ]
                []

        divHtml =
            div
                [ style
                    [ ( "font-size", st.fontSize )
                    , ( "width", getWidth config )
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
        div []
            [ div
                [ classList
                    [ ( "mdc-textfield mdc-textfield--upgraded", True )
                    , ( "mdc-textfield--focused", isFocused )
                    , ( "mdc-textfield--disabled", config.disabled )
                    , ( "ui-textfield--readonly", config.readonly )
                    , ( "mdc-textfield--fullwidth", False )
                    , ( "mdc-textfield--invalid", config.invalid )
                    ]
                , Events.onFocus <| Focus
                , Events.onBlur <| Blur
                , style
                    [ ( "height", st.height )
                    , ( "position", "relative" )
                    , ( "width", getWidth config )
                    ]
                ]
                [ contentHtml
                , label
                    [ classList
                        [ ( "mdc-textfield__label mdc-typography", True )
                        , ( "mdc-textfield__label--float-above"
                          , isFocused
                                || (String.length value > 0)
                          )
                        ]
                    , style
                        [ ( "bottom", st.labelBottom )
                        , ( "font-size", st.labelFontSize )
                        ]
                    ]
                    (case config.labelText of
                        Just label ->
                            [ text <|
                                label
                                    ++ (if config.required then
                                            " *"
                                        else
                                            ""
                                       )
                            ]

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
            , p [ classList errorClasses, style [ ( "max-width", getWidth config ) ] ] [ text (config.errorText) ]
            ]


(!) : Model -> List (Cmd m) -> ( Model, Cmd m, TextfieldEvent )
(!) m cs =
    ( m, Cmd.batch cs, NoChange )
