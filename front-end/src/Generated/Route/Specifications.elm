module Generated.Route.Specifications exposing
    ( Route(..)
    , routes
    , toPath
    , AllParams
    , NewParams
    )


import Application.Route as Route



type alias AllParams =
    ()


type alias NewParams =
    ()




 
type Route
    = All AllParams
    | New NewParams


routes =
    [ Route.path "all" All
    , Route.path "new" New
    ]


toPath route =
    case route of
        All _ ->
            "/all"

        New _ ->
            "/new"

