module Saf exposing (..)

import Html


rEmpty : Restriction
rEmpty =
    Restriction [] [] [] []


type alias Restriction =
    { deviceType : List String
    , manufacturer : List String
    , model : List String
    , os : List String
    }


type alias Node =
    { id : String
    , title : String
    , restrictions : Restriction
    }


type alias Filter =
    { deviceType : List Node
    , manufacturer : List Node
    , model : List Node
    , os : List Node

    -- , browser : List Node
    -- , country : List Node
    -- , settlement : List Node
    -- , language : List Node
    }


gfilter : Filter
gfilter =
    { deviceType =
        [ Node "desktop" "ПК" rEmpty
        , Node "mobile" "mobile" rEmpty
        , Node "tablet" "tablet" rEmpty
        , Node "tv" "tv" rEmpty
        ]
    , manufacturer =
        [ Node "4good" "4good" (Restriction [ "mobile", "desktop" ] [] [] [])
        , Node "acer" "acer" (Restriction [ "mobile", "desktop" ] [] [] [])
        ]
    , model =
        [ Node "201487" "201487" (Restriction [ "mobile" ] [ "acer" ] [] [])
        ]
    , os =
        [ Node "Android" "Android" (Restriction [ "mobile" ] [ "acer" ] [] [])
        ]
    }


getDeviceTypes : Filter -> List String
getDeviceTypes filter =
    List.map .title filter.deviceType


getManufacturers : String -> Filter -> List Node
getManufacturers deviceTypeId filter =
    let
        resFilter { restrictions } =
            List.member deviceTypeId restrictions.deviceType
    in
        List.filter resFilter filter.manufacturer


getModels : String -> String -> Filter -> List Node
getModels deviceTypeId manufacturerId filter =
    let
        manufacturers =
            getManufacturers deviceTypeId filter

        manufacturerFilter { restrictions } =
            case manufacturerId of
                "" ->
                    True

                _ ->
                    List.member manufacturerId restrictions.manufacturer
    in
        case manufacturers of
            [] ->
                []

            array ->
                List.filter manufacturerFilter filter.model


getOss : String -> String -> String -> Filter -> List Node
getOss deviceTypeId manufacturerId modelId filter =
    let
        models =
            getModels deviceTypeId manufacturerId filter

        modelFilter { restrictions } =
            case modelId of
                "" ->
                    True

                _ ->
                    List.member modelId restrictions.model
    in
        case models of
            [] ->
                []

            array ->
                List.filter modelFilter filter.os


main : Html.Html msg
main =
    Html.text <| toString <| List.map .title (getOss "mobile" "acer" "" gfilter)
