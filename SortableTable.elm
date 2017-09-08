module Main exposing (..)

import Html exposing (Html, div, h1, input, text, Attribute, span, button)
import Html.Attributes exposing (placeholder, class, checked, type_, style)
import Html.Events exposing (onInput, onClick, onWithOptions)
import Json.Decode as Json
import Table exposing (defaultCustomizations)
import Checkbox
import Icons.ArrowUpward as ArrowUpward
import MenuUi as Menu exposing (menuView)
import DOM exposing (target, offsetWidth)
import Debug
import Button
import Mouse


main =
    Html.program
        { init = init presidents
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { people : List Person
    , tableState : Table.State
    , query : String
    , geometry : Maybe Geometry
    , opened : Bool
    , top : Float
    , left : Float
    }


init : List Person -> ( Model, Cmd Msg )
init people =
    let
        model =
            { people = people
            , tableState = Table.initialSort "Year"
            , query = ""
            , geometry = Nothing
            , opened = False
            , top = 0
            , left = 0
            }
    in
        ( model, Cmd.none )



-- UPDATE


type Msg
    = SetQuery String
    | ToggleSelected String
    | ToggleMenu String Geometry
    | SetTableState Table.State
    | Click Mouse.Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetQuery newQuery ->
            ( { model | query = newQuery }
            , Cmd.none
            )

        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            )

        ToggleSelected name ->
            if model.opened then
                ( model, Cmd.none )
            else
                ( { model | people = List.map (toggleSelected name) model.people }
                , Cmd.none
                )

        ToggleMenu name geom ->
            ( { model
                | opened = True
                , geometry = Just geom
                , top = geom.button.bounds.top
                , left = geom.button.bounds.left - 170 + geom.button.bounds.width
                , people = List.map (toggleMenuClicked name) model.people
              }
            , Cmd.none
            )

        Click pos ->
            let
                _ =
                    Debug.log "pos" pos
            in
                case model.geometry of
                    Just geom ->
                        let
                            inside { x, y } { top, left, width, height } =
                                (left <= toFloat x)
                                    && (toFloat x <= left + width)
                                    && (top <= toFloat y)
                                    && (toFloat y <= top + height)
                        in
                            if inside pos geom.menu.bounds then
                                ( model, Cmd.none )
                            else
                                ( { model
                                    | opened = False
                                    , top = geom.button.bounds.top
                                    , left = geom.button.bounds.left - 170 + geom.button.bounds.width
                                    , people = List.map (\p -> { p | menuClicked = False }) model.people
                                  }
                                , Cmd.none
                                )

                    Nothing ->
                        ( model, Cmd.none )


toggleSelected : String -> Person -> Person
toggleSelected name person =
    if person.name == name then
        { person | selected = not person.selected }
    else
        person


toggleMenuClicked : String -> Person -> Person
toggleMenuClicked name person =
    if person.name == name then
        { person | menuClicked = True }
    else
        { person | menuClicked = False }



-- VIEW


view : Model -> Html Msg
view { people, tableState, query, left, top, opened } =
    let
        lowerQuery =
            String.toLower query

        acceptablePeople =
            List.filter (String.contains lowerQuery << String.toLower << .name) people
    in
        div [ style [ ( "width", "700px" ) ] ]
            [ div []
                [ input [ placeholder "Search by Name", onInput SetQuery ] []
                , Table.view config tableState acceptablePeople
                , Menu.menuView opened top left
                ]
            , Html.node "link"
                [ Html.Attributes.rel "stylesheet"
                , Html.Attributes.href "style.css?v.11.1"
                ]
                []
            , Html.node "link"
                [ Html.Attributes.rel "stylesheet"
                , Html.Attributes.href "https://unpkg.com/material-components-web@latest/dist/material-components-web.min.css"
                ]
                []
            , Html.node "link"
                [ Html.Attributes.rel "stylesheet"
                , Html.Attributes.href "https://fonts.googleapis.com/icon?family=Material+Icons"
                ]
                []
            ]


simpleThead : List ( String, Table.Status, Attribute msg ) -> Table.HtmlDetails msg
simpleThead headers =
    let
        indexed =
            List.indexedMap (,) headers
    in
        Table.HtmlDetails [] (List.map simpleTheadHelp indexed)


simpleTheadHelp : ( Int, ( String, Table.Status, Attribute msg ) ) -> Html msg
simpleTheadHelp ( index, ( name, status, onClick ) ) =
    let
        isFirstIndex =
            index == 1

        content =
            case status of
                Table.Unsortable ->
                    [ Html.text name ]

                Table.Sortable selected ->
                    [ Html.text name ]

                Table.Reversible Nothing ->
                    case isFirstIndex of
                        True ->
                            [ Html.span [ class "c54 arrow-button" ]
                                [ Html.text name
                                , ArrowUpward.view "arrow-icon arrow-hidden"
                                ]
                            ]

                        False ->
                            [ Html.span [ class "c54 arrow-button" ]
                                [ ArrowUpward.view "arrow-icon arrow-hidden"
                                , Html.text name
                                ]
                            ]

                Table.Reversible (Just isReversed) ->
                    case isFirstIndex of
                        True ->
                            [ Html.span [ class "c54 arrow-button bold" ]
                                [ Html.text name
                                , if isReversed then
                                    ArrowUpward.view "arrow-icon asc"
                                  else
                                    ArrowUpward.view "arrow-icon desc"
                                ]
                            ]

                        False ->
                            [ Html.span [ class "c54 arrow-button bold" ]
                                [ if isReversed then
                                    ArrowUpward.view "arrow-icon asc"
                                  else
                                    ArrowUpward.view "arrow-icon desc"
                                , Html.text name
                                ]
                            ]
    in
        Html.th [ onClick ] content


checkboxColumn : Table.Column Person Msg
checkboxColumn =
    Table.veryCustomColumn
        { name = ""
        , viewData = viewCheckbox
        , sorter = Table.unsortable
        }


viewCheckbox : Person -> Table.HtmlDetails Msg
viewCheckbox { selected } =
    Table.HtmlDetails []
        [ Checkbox.view selected ]


menuColumn : Table.Column Person Msg
menuColumn =
    Table.veryCustomColumn
        { name = ""
        , viewData = viewMenu
        , sorter = Table.unsortable
        }



-- Custom event handler


type alias Geometry =
    { button : Element
    , menu : Element
    }


type alias Element =
    { offsetTop : Float
    , offsetLeft : Float
    , offsetHeight : Float
    , bounds : DOM.Rectangle
    }


{-| Decode an Element
-}
element : Json.Decoder Element
element =
    Json.map4 Element
        DOM.offsetTop
        DOM.offsetLeft
        DOM.offsetHeight
        DOM.boundingClientRect


{-| Decode Geometry
-}
decoder : Json.Decoder Geometry
decoder =
    Json.map2 Geometry
        (DOM.target element)
        (DOM.target (DOM.nextSibling element))


onMenuClick : (Geometry -> msg) -> Attribute msg
onMenuClick msg =
    onWithOptions "click" { stopPropagation = True, preventDefault = True } (Json.map msg Menu.decoder)


viewMenu : Person -> Table.HtmlDetails Msg
viewMenu p =
    Table.HtmlDetails []
        [ Button.renderButton [ onMenuClick (ToggleMenu p.name) ]
        , Menu.menuView False 0 0
        ]


config : Table.Config Person Msg
config =
    Table.customConfig
        { toId = .name
        , toMsg = SetTableState
        , columns =
            [ checkboxColumn
            , Table.stringColumn "Name" .name
            , Table.stringColumn "City" .city
            , Table.intColumn "Year" .year
            , Table.stringColumn "State" .state
            , menuColumn
            ]
        , customizations =
            { defaultCustomizations
                | tableAttrs = [ class "elm-table" ]
                , rowAttrs = \d -> [ onClick (ToggleSelected d.name) ]
                , thead = simpleThead
            }
        }



-- PEOPLE


type alias Person =
    { name : String
    , year : Int
    , city : String
    , state : String
    , selected : Bool
    , menuClicked : Bool
    }


presidents : List Person
presidents =
    [ Person "George Washington" 1732 "Westmoreland County" "Virginia" False False
    , Person "John Adams" 1735 "Braintree" "Massachusetts" False False
    , Person "Thomas Jefferson" 1743 "Shadwell" "Virginia" False False
    , Person "James Madison" 1751 "Port Conway" "Virginia" False False
    , Person "James Monroe" 1758 "Monroe Hall" "Virginia" False False
    , Person "Andrew Jackson" 1767 "Waxhaws Region" "South/North Carolina" False False
    , Person "John Quincy Adams" 1767 "Braintree" "Massachusetts" False False
    , Person "William Henry Harrison" 1773 "Charles City County" "Virginia" False False
    , Person "Martin Van Buren" 1782 "Kinderhook" "New York" False False
    , Person "Zachary Taylor" 1784 "Barboursville" "Virginia" False False
    , Person "John Tyler" 1790 "Charles City County" "Virginia" False False
    , Person "James Buchanan" 1791 "Cove Gap" "Pennsylvania" False False
    , Person "James K. Polk" 1795 "Pineville" "North Carolina" False False
    , Person "Millard Fillmore" 1800 "Summerhill" "New York" False False
    , Person "Franklin Pierce" 1804 "Hillsborough" "New Hampshire" False False
    , Person "Andrew Johnson" 1808 "Raleigh" "North Carolina" False False
    , Person "Abraham Lincoln" 1809 "Sinking spring" "Kentucky" False False
    , Person "Ulysses S. Grant" 1822 "Point Pleasant" "Ohio" False False
    , Person "Rutherford B. Hayes" 1822 "Delaware" "Ohio" False False
    , Person "Chester A. Arthur" 1829 "Fairfield" "Vermont" False False
    , Person "James A. Garfield" 1831 "Moreland Hills" "Ohio" False False
    , Person "Benjamin Harrison" 1833 "North Bend" "Ohio" False False
    , Person "Grover Cleveland" 1837 "Caldwell" "New Jersey" False False
    , Person "William McKinley" 1843 "Niles" "Ohio" False False
    , Person "Woodrow Wilson" 1856 "Staunton" "Virginia" False False
    , Person "William Howard Taft" 1857 "Cincinnati" "Ohio" False False
    , Person "Theodore Roosevelt" 1858 "New York City" "New York" False False
    , Person "Warren G. Harding" 1865 "Blooming Grove" "Ohio" False False
    , Person "Calvin Coolidge" 1872 "Plymouth" "Vermont" False False
    , Person "Herbert Hoover" 1874 "West Branch" "Iowa" False False
    , Person "Franklin D. Roosevelt" 1882 "Hyde Park" "New York" False False
    , Person "Harry S. Truman" 1884 "Lamar" "Missouri" False False
    , Person "Dwight D. Eisenhower" 1890 "Denison" "Texas" False False
    , Person "Lyndon B. Johnson" 1908 "Stonewall" "Texas" False False
    , Person "Ronald Reagan" 1911 "Tampico" "Illinois" False False
    , Person "Richard M. Nixon" 1913 "Yorba Linda" "California" False False
    , Person "Gerald R. Ford" 1913 "Omaha" "Nebraska" False False
    , Person "John F. Kennedy" 1917 "Brookline" "Massachusetts" False False
    , Person "George H. W. Bush" 1924 "Milton" "Massachusetts" False False
    , Person "Jimmy Carter" 1924 "Plains" "Georgia" False False
    , Person "George W. Bush" 1946 "New Haven" "Connecticut" False False
    , Person "Bill Clinton" 1946 "Hope" "Arkansas" False False
    , Person "Barack Obama" 1961 "Honolulu" "Hawaii" False False
    , Person "Donald Trump" 1946 "New York City" "New York" False False
    ]



-- SUBSCRIBTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.opened == True then
            Mouse.clicks Click
          else
            Sub.none
        ]
