module Generated.Pages exposing
    ( Model
    , Msg
    , page
    )

import Application.Page as Page
import Generated.Pages.Specifications as Specifications
import Generated.Route as Route
import Element
import Layouts.Main as Layout
import Pages.Designs as Designs
import Pages.Index as Index
import Pages.NotFound as NotFound
import Pages.ReferenceSets as ReferenceSets
import Pages.Settings as Settings


type Model
    = DesignsModel Designs.Model
    | IndexModel Index.Model
    | NotFoundModel NotFound.Model
    | ReferenceSetsModel ReferenceSets.Model
    | SettingsModel Settings.Model
    | SpecificationsModel Specifications.Model


type Msg
    = DesignsMsg Designs.Msg
    | IndexMsg Index.Msg
    | NotFoundMsg NotFound.Msg
    | ReferenceSetsMsg ReferenceSets.Msg
    | SettingsMsg Settings.Msg
    | SpecificationsMsg Specifications.Msg


page =
    Page.layout
        { map = Element.map
        , view = Layout.view
        , pages =
            { init = init
            , update = update
            , bundle = bundle
            }
        }


designs =
        Page.recipe Designs.page
        { toModel = DesignsModel
        , toMsg = DesignsMsg
        , map = Element.map
        }


index =
        Page.recipe Index.page
        { toModel = IndexModel
        , toMsg = IndexMsg
        , map = Element.map
        }


notFound =
        Page.recipe NotFound.page
        { toModel = NotFoundModel
        , toMsg = NotFoundMsg
        , map = Element.map
        }


referenceSets =
        Page.recipe ReferenceSets.page
        { toModel = ReferenceSetsModel
        , toMsg = ReferenceSetsMsg
        , map = Element.map
        }


settings =
        Page.recipe Settings.page
        { toModel = SettingsModel
        , toMsg = SettingsMsg
        , map = Element.map
        }


specifications =
        Page.recipe Specifications.page
        { toModel = SpecificationsModel
        , toMsg = SpecificationsMsg
        , map = Element.map
        }


init route_ =
    case route_ of
        Route.Designs route ->
            designs.init route

        Route.Index route ->
            index.init route

        Route.NotFound route ->
            notFound.init route

        Route.ReferenceSets route ->
            referenceSets.init route

        Route.Settings route ->
            settings.init route

        Route.Specifications route ->
            specifications.init route


update msg_ model_ =
    case ( msg_, model_ ) of
        ( DesignsMsg msg, DesignsModel model ) ->
            designs.update msg model

        ( IndexMsg msg, IndexModel model ) ->
            index.update msg model

        ( NotFoundMsg msg, NotFoundModel model ) ->
            notFound.update msg model

        ( ReferenceSetsMsg msg, ReferenceSetsModel model ) ->
            referenceSets.update msg model

        ( SettingsMsg msg, SettingsModel model ) ->
            settings.update msg model

        ( SpecificationsMsg msg, SpecificationsModel model ) ->
            specifications.update msg model

        _ ->
            Page.keep model_


bundle model_ =
    case model_ of
        DesignsModel model ->
            designs.bundle model

        IndexModel model ->
            index.bundle model

        NotFoundModel model ->
            notFound.bundle model

        ReferenceSetsModel model ->
            referenceSets.bundle model

        SettingsModel model ->
            settings.bundle model

        SpecificationsModel model ->
            specifications.bundle model

