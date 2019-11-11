module Generated.Route exposing
    ( Route(..)
    , routes
    , toPath
    , DesignsParams
    , IndexParams
    , NotFoundParams
    , ReferenceSetsParams
    , SettingsParams
    , SpecificationsParams
    )


import Application.Route as Route
import Generated.Route.Specifications as Specifications


type alias DesignsParams =
    ()


type alias IndexParams =
    ()


type alias NotFoundParams =
    ()


type alias ReferenceSetsParams =
    ()


type alias SettingsParams =
    ()


type alias SpecificationsParams =
    Specifications.Route

 
type Route
    = Designs DesignsParams
    | Index IndexParams
    | NotFound NotFoundParams
    | ReferenceSets ReferenceSetsParams
    | Settings SettingsParams
    | Specifications SpecificationsParams


routes =
    [ Route.path "designs" Designs
    , Route.index Index
    , Route.path "not-found" NotFound
    , Route.path "reference-sets" ReferenceSets
    , Route.path "settings" Settings
    , Route.folder "specifications" Specifications Specifications.routes
    ]


toPath route =
    case route of
        Designs _ ->
            "/designs"

        Index _ ->
            "/"

        NotFound _ ->
            "/not-found"

        ReferenceSets _ ->
            "/reference-sets"

        Settings _ ->
            "/settings"

        Specifications route_ ->
            "/specifications" ++ Specifications.toPath route_

