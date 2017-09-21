module MyDatePicker
    exposing
        ( Msg
        , Settings
        , DateEvent(..)
        , DatePicker
        , defaultSettings
        , init
          -- , initFromDate
          -- , initFromDates
        , update
        , view
        , pick
        , isOpen
        , between
        , moreOrLess
        , off
        , from
        , to
        , focusedDate
        )

import Date exposing (Date, Day(..), Month, day, month, year)
import MyDatePickerDate exposing (..)
import Icons.Icon as Icon
import Html exposing (..)
import Html.Attributes as Attrs exposing (href, placeholder, tabindex, type_, value, selected)
import Html.Events exposing (on, onBlur, onClick, onInput, onFocus, onWithOptions, targetValue)
import Html.Keyed
import Json.Decode as Json
import Task
import Textfield


type Msg
    = CurrentDate Date
    | ChangeFocus Date
    | Pick (Maybe Date)
    | Text String
    | SubmitText
    | Focus
    | Blur
    | MouseDown
    | MouseUp
    | TextfieldMsg Textfield.Msg


type alias Settings =
    { placeholder : String
    , classNamespace : String
    , inputClassList : List ( String, Bool )
    , inputName : Maybe String
    , inputId : Maybe String
    , inputAttributes : List (Html.Attribute Msg)
    , isDisabled : Date -> Bool
    , parser : String -> Result String Date
    , dateFormatter : Date -> String
    , dayFormatter : Day -> String
    , monthFormatter : Month -> String
    , yearFormatter : Int -> String
    , cellFormatter : String -> Html Msg
    , firstDayOfWeek : Day
    , changeYear : YearRange
    }


type alias Model =
    { open : Bool
    , forceOpen : Bool
    , focused : Maybe Date
    , inputText : Maybe String
    , today : Date
    , textfield : Textfield.Model
    }


type DatePicker
    = DatePicker Model


defaultSettings : Settings
defaultSettings =
    { placeholder = "Please pick a date..."
    , classNamespace = "elm-datepicker--"
    , inputClassList = []
    , inputName = Nothing
    , inputId = Nothing
    , inputAttributes =
        [ Attrs.required False
        ]
    , isDisabled = always False
    , parser = Date.fromString
    , dateFormatter = formatDate
    , dayFormatter = formatDay
    , monthFormatter = formatMonth
    , yearFormatter = toString
    , cellFormatter = formatCell
    , firstDayOfWeek = Sun
    , changeYear = off
    }


yearRangeActive : YearRange -> Bool
yearRangeActive yearRange =
    yearRange /= Off


between : Int -> Int -> YearRange
between start end =
    if start > end then
        Between end start
    else
        Between start end


moreOrLess : Int -> YearRange
moreOrLess range =
    MoreOrLess range


from : Int -> YearRange
from year =
    From year


to : Int -> YearRange
to year =
    To year


off : YearRange
off =
    Off


formatCell : String -> Html Msg
formatCell day =
    text day


defaultModel : Model
defaultModel =
    { open = True
    , forceOpen = True
    , focused = Just initDate
    , inputText = Nothing
    , today = initDate
    , textfield = Textfield.defaultModel
    }


init : ( DatePicker, Cmd Msg )
init =
    ( DatePicker defaultModel
    , Task.perform CurrentDate Date.now
    )


prepareDates : Date -> Day -> { currentMonth : Date, currentDates : List Date }
prepareDates date firstDayOfWeek =
    let
        start =
            firstOfMonth date |> subDays 6

        end =
            nextMonth date |> addDays 6
    in
        { currentMonth = date
        , currentDates = datesInRange firstDayOfWeek start end
        }


isOpen : DatePicker -> Bool
isOpen (DatePicker model) =
    True


focusedDate : DatePicker -> Maybe Date
focusedDate (DatePicker model) =
    model.focused


textfieldConfig : Textfield.Config
textfieldConfig =
    let
        dc =
            Textfield.defaultConfig
    in
        { dc
            | defaultValue = Nothing
            , readonly = False
            , labelText = Just "Дата"
        }


type DateEvent
    = NoChange
    | Changed (Maybe Date)


update : Settings -> Msg -> DatePicker -> ( DatePicker, Cmd Msg, DateEvent )
update settings msg (DatePicker model) =
    case msg of
        TextfieldMsg m ->
            let
                ( newTextfieldModel, _ ) =
                    Textfield.update TextfieldMsg m model.textfield textfieldConfig
            in
                { model | textfield = newTextfieldModel } ! []

        CurrentDate date ->
            { model | focused = Just date, today = date } ! []

        ChangeFocus date ->
            { model | focused = Just date } ! []

        Pick date ->
            ( DatePicker <|
                { model
                    | open = False
                    , inputText = Nothing
                    , focused = Nothing
                }
            , Cmd.none
            , Changed date
            )

        Text text ->
            { model | inputText = Just text } ! []

        SubmitText ->
            let
                isWhitespace =
                    String.trim >> String.isEmpty

                dateEvent =
                    let
                        text =
                            model.inputText ?> ""
                    in
                        if isWhitespace text then
                            Changed Nothing
                        else
                            text
                                |> settings.parser
                                |> Result.map
                                    (Changed
                                        << (\date ->
                                                if settings.isDisabled date then
                                                    Nothing
                                                else
                                                    Just date
                                           )
                                    )
                                |> Result.withDefault NoChange
            in
                ( DatePicker <|
                    { model
                        | inputText =
                            case dateEvent of
                                Changed _ ->
                                    Nothing

                                NoChange ->
                                    model.inputText
                        , focused =
                            case dateEvent of
                                Changed _ ->
                                    Nothing

                                NoChange ->
                                    model.focused
                    }
                , Cmd.none
                , dateEvent
                )

        Focus ->
            { model | open = True, forceOpen = False } ! []

        Blur ->
            { model | open = model.forceOpen } ! []

        MouseDown ->
            { model | forceOpen = True } ! []

        MouseUp ->
            { model | forceOpen = False } ! []


pick : Maybe Date -> Msg
pick =
    Pick


view : Maybe Date -> Settings -> DatePicker -> Html Msg
view pickedDate settings (DatePicker ({ open } as model)) =
    let
        class =
            mkClass settings

        potentialInputId =
            settings.inputId
                |> Maybe.map Attrs.id
                |> (List.singleton >> List.filterMap identity)

        inputClasses =
            [ ( settings.classNamespace ++ "input", True ) ]
                ++ settings.inputClassList

        inputCommon xs =
            input
                ([ Attrs.classList inputClasses
                 , Attrs.name (settings.inputName ?> "")
                 , type_ "text"
                 , on "change" (Json.succeed SubmitText)
                 , onInput Text
                 , onBlur Blur
                 , onClick Focus
                 , onFocus Focus
                 ]
                    ++ settings.inputAttributes
                    ++ potentialInputId
                    ++ xs
                )
                []

        dateInput =
            inputCommon
                [ placeholder settings.placeholder
                , model.inputText
                    |> Maybe.withDefault
                        (Maybe.map settings.dateFormatter pickedDate
                            |> Maybe.withDefault ""
                        )
                    |> value
                ]

        -- dateInput =
        --     Textfield.view (TextfieldMsg)
        --         model.textfield
        --         textfieldConfig
    in
        div [ class "container" ]
            [ dateInput
            , if open then
                datePicker pickedDate settings model
              else
                text ""
            ]


datePicker : Maybe Date -> Settings -> Model -> Html Msg
datePicker pickedDate settings ({ focused, today } as model) =
    let
        currentDate =
            focused ??> pickedDate ?> today

        { currentMonth, currentDates } =
            prepareDates currentDate settings.firstDayOfWeek

        class =
            mkClass settings

        classList =
            mkClassList settings

        firstDay =
            settings.firstDayOfWeek

        arrow className message =
            a
                [ class className
                , href "javascript:;"
                , onClick message
                , tabindex -1
                ]
                []

        weekDay d =
            span [ class "weekday" ] [ text <| settings.dayFormatter d ]

        picked d =
            pickedDate
                |> Maybe.map
                    (dateTuple >> (==) (dateTuple d))
                |> Maybe.withDefault False

        day d =
            let
                disabled =
                    settings.isDisabled d

                otherMonth =
                    month currentMonth /= month d

                classList_ =
                    [ ( "day", True )
                    , ( "disabled", disabled )
                    , ( "picked", picked d )
                    , ( "today", dateTuple d == dateTuple currentDate )
                    , ( "other-month", otherMonth )
                    ]

                text =
                    settings.cellFormatter <| toString <| Date.day d

                element =
                    if otherMonth then
                        span ([ classList classList_ ]) [ text ]
                    else
                        button ([ classList classList_ ] ++ props)
                            [ div [ class "day-0" ] []
                            , span [ class "day-text" ] [ text ]
                            ]

                props =
                    if not disabled then
                        [ onClick (Pick (Just d)) ]
                    else
                        []
            in
                element

        row days =
            div [ class "date-row" ] (List.map day days)

        days =
            List.map row (groupDates currentDates)

        onPicker ev =
            Json.succeed
                >> onWithOptions ev
                    { preventDefault = False
                    , stopPropagation = True
                    }

        onChange handler =
            on "change" <| Json.map handler targetValue

        isCurrentYear selectedYear =
            year currentMonth == selectedYear

        yearOption index selectedYear =
            ( toString index
            , option [ value (toString selectedYear), selected (isCurrentYear selectedYear) ]
                [ text <| toString selectedYear ]
            )

        dropdownYear =
            Html.Keyed.node "select"
                [ onChange (newYear currentDate >> ChangeFocus), class "year-menu" ]
                (List.indexedMap yearOption
                    (yearRange { focused = currentDate, currentMonth = currentMonth } settings.changeYear)
                )
    in
        div
            [ class "picker"
            , onPicker "mousedown" MouseDown
            , onPicker "mouseup" MouseUp
            ]
            [ div [ class "picker-header" ]
                [ div [ class "year" ]
                    [ text <| settings.yearFormatter <| year currentMonth ]
                , div [ class "current-date" ]
                    [ text <| settings.dateFormatter <| today ]
                ]
            , div [ class "body" ]
                [ div [ class "dates-and-month-container" ]
                    [ div [ class "dates-and-month-container-0" ]
                        [ div [ class "month-container" ]
                            [ Icon.asButton "keyboard_arrow_left"
                                [ onClick <| ChangeFocus (prevMonth currentDate)
                                ]
                            , div [ class "month-title-container" ]
                                [ div [] [ text <| settings.monthFormatter <| month currentMonth ]
                                ]
                            , Icon.asButton "keyboard_arrow_right"
                                [ onClick <| ChangeFocus (nextMonth currentDate)
                                ]
                            ]
                        , div [ class "weekdays" ]
                            [ weekDay <| firstDay
                            , weekDay <| addDows 1 firstDay
                            , weekDay <| addDows 2 firstDay
                            , weekDay <| addDows 3 firstDay
                            , weekDay <| addDows 4 firstDay
                            , weekDay <| addDows 5 firstDay
                            , weekDay <| addDows 6 firstDay
                            ]
                        , div [ class "dates" ]
                            [ div [ class "dates-0" ]
                                [ div [ class "dates-0-0" ] days
                                ]
                            ]
                        ]
                    ]
                ]
            ]


{-| Turn a list of dates into a list of date rows with 7 columns per
row representing each day of the week.
-}
groupDates : List Date -> List (List Date)
groupDates dates =
    let
        go i xs racc acc =
            case xs of
                [] ->
                    List.reverse acc

                x :: xs ->
                    if i == 6 then
                        go 0 xs [] (List.reverse (x :: racc) :: acc)
                    else
                        go (i + 1) xs (x :: racc) acc
    in
        go 0 dates [] []


mkClass : Settings -> String -> Html.Attribute msg
mkClass { classNamespace } c =
    Attrs.class (classNamespace ++ c)


mkClassList : Settings -> List ( String, Bool ) -> Html.Attribute msg
mkClassList { classNamespace } cs =
    List.map (\( c, b ) -> ( classNamespace ++ c, b )) cs
        |> Attrs.classList


(!) : Model -> List (Cmd Msg) -> ( DatePicker, Cmd Msg, DateEvent )
(!) m cs =
    ( DatePicker m, Cmd.batch cs, NoChange )


(?>) : Maybe a -> a -> a
(?>) =
    flip Maybe.withDefault


(??>) : Maybe a -> Maybe a -> Maybe a
(??>) first default =
    case first of
        Just val ->
            Just val

        Nothing ->
            default
