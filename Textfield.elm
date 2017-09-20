module Textfield
    exposing
        ( view
        , Model
        , defaultModel
        , Msg
        , update
        , Config
        , defaultConfig
        )

import Html exposing (Html, span, input, label, text, div, button, Attribute)
import Html.Attributes as Attr exposing (class, classList, style)
import Html.Events as Events
import Internal.Textfield exposing (Msg(..))
import Char
import Utils exposing (..)


type alias Model =
    { isFocused : Bool
    , isDirty : Bool
    , value : Maybe String
    }


defaultModel : Model
defaultModel =
    { isFocused = False
    , isDirty = False
    , value = Nothing
    }


type alias Msg =
    Internal.Textfield.Msg


update : (Msg -> m) -> Msg -> Model -> Config -> ( Model, Cmd m )
update lift msg model config =
    case msg of
        Input str ->
            let
                dirty =
                    str /= ""

                allDigits =
                    String.all Char.isDigit str

                numberedValue =
                    if allDigits then
                        Just str
                    else
                        model.value

                newValue =
                    if config.numbered then
                        numberedValue
                    else
                        Just str
            in
                ( { model | value = newValue, isDirty = dirty }, Cmd.none )

        Blur ->
            ( { model | isFocused = False }, Cmd.none )

        Focus ->
            ( { model | isFocused = True }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


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
    , numbered : Bool
    , readonly : Bool
    , plural : Maybe Some
    }


defaultConfig : Config
defaultConfig =
    { labelText = Just "Сумма"
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
    }


view : (Msg -> m) -> Model -> Config -> Html m
view lift model config =
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
            model.value
                |> Maybe.withDefault
                    (config.defaultValue
                        |> Maybe.withDefault ""
                    )

        extra =
            config.extra |> Maybe.withDefault ""

        intValue =
            String.toInt value |> Result.withDefault 0

        pl =
            Maybe.map (flip Utils.pluralize intValue) config.plural
                |> Maybe.withDefault ""

        _ =
            Debug.log "pl" (value ++ " " ++ pl)

        inputHtml =
            input
                [ Attr.type_ "text"
                , style
                    [ ( "font-size", fontSize )
                    ]
                , classList
                    [ ( "mdc-textfield__input", True )
                    ]
                , Events.onFocus <| lift Focus
                , Events.onBlur <| lift Blur
                , Events.onInput (Input >> lift)
                , Attr.value value
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
                [ text value ]

        contentHtml =
            if config.readonly then
                divHtml
            else
                inputHtml
    in
        div
            [ classList
                [ ( "mdc-textfield mdc-textfield--upgraded", True )
                , ( "mdc-textfield--focused", isFocused )
                , ( "mdc-textfield--disabled", config.disabled )
                , ( "mdc-textfield--fullwidth", config.fullWidth )
                , ( "mdc-textfield--invalid", False )
                ]
            , Events.onFocus <| lift Focus
            , Events.onBlur <| lift Blur
            , style [ ( "height", height ) ]
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
