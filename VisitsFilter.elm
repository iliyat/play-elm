module VisitsFilter exposing (..)

import Html
import Dict
import Set


type alias Browser =
    String


type alias OS =
    { name : String
    , browser : List Browser
    }


type alias Model =
    { name : String
    , os : List OS
    }


type alias Manufacturer =
    { name : String
    , model : List Model
    }


type alias DeviceType =
    { name : String
    , manufacturer : List Manufacturer
    , os : List OS
    , model : List Model
    , browser : List Browser
    }


type alias Country =
    { name : String
    , settlement : List String
    }


type alias FilterType =
    { deviceType : List DeviceType
    , country : List Country
    }


filters_ : FilterType
filters_ =
    FilterType
        [ DeviceType "ПК"
            [ Manufacturer "Asus"
                [ Model "Asus zenbook"
                    [ OS "Windows7"
                        [ "IE", "Chrome" ]
                    ]
                ]
            , Manufacturer "Acer"
                [ Model "Afd"
                    [ OS "Windows7"
                        [ "IE", "Chrome" ]
                    ]
                ]
            ]
            [ OS "Windows7" [ "IE", "Chrome1" ] ]
            []
            []
        , DeviceType "Мобильные"
            []
            [ OS "Android"
                [ "Browser", "Chrome" ]
            ]
            []
            []
        ]
        [ Country "Россия" [ "г Москва" ] ]


getDeviceTypes : FilterType -> List String
getDeviceTypes filter =
    List.map (\dt -> dt.name) filter.deviceType


getManufacturers : String -> FilterType -> List Manufacturer
getManufacturers deviceType filter =
    let
        filtered =
            List.filter (\dt -> dt.name == deviceType) filter.deviceType

        foundDeviceType =
            List.head filtered |> Maybe.withDefault (DeviceType "" [] [] [] [])
    in
        foundDeviceType.manufacturer


getModels : String -> String -> FilterType -> List String
getModels deviceTypeName manufacturerName filter =
    let
        filtered =
            List.filter (\m -> m.name == manufacturerName) (getManufacturers deviceTypeName filter)

        foundModel =
            List.head filtered |> Maybe.withDefault (Manufacturer "" [])
    in
        List.map .name foundModel.model

getNextFilterValues : String -> FilterType -> String 
getNextFilterValues deviceTypeName filter =
  let
      nextList = getManufacturers deviceTypeName filter


main : Html.Html msg
main =
    Html.text <| toString <| getModels "ПК" "Asus" filters_





