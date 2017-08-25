module Main exposing (..)

import Html


type alias Node =
    { value : String
    , children : Children
    }


type Children
    = Children (List Node)


filters : Node
filters =
    Node "root"
        (Children
            ([ Node "ПК"
                (Children
                    [ Node ""
                        (Children
                            [ Node "Zenbook-1"
                                (Children
                                    [ Node "Windows7"
                                        (Children
                                            [ Node "Safari" (Children []), Node "Chrome" (Children []) ]
                                        )
                                    ]
                                )
                            ]
                        )
                    ]
                )
             ]
            )
        )


getChildrenValues : Children -> List String
getChildrenValues children =
    case children of
        Children [] ->
            [ "nope" ]

        Children array ->
            List.map .value array


childrenAt : Int -> Int -> Node -> List String
childrenAt currentDepth depth node =
    case node.children of
        Children [] ->
            [ "not found" ]

        Children array ->
            if currentDepth == depth then
                getChildrenValues node.children
            else
                List.map (childrenAt (currentDepth + 1) depth) array |> List.concat


main : Html.Html msg
main =
    Html.text <| toString <| childrenAt 0 2 filters
