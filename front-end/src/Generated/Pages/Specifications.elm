module Generated.Pages.Specifications exposing
    ( Model
    , Msg
    , page
    )

import Application.Page as Page

import Generated.Route.Specifications as Route
import Element
import Layouts.Specifications as Layout
import Pages.Specifications.All as All
import Pages.Specifications.New as New


type Model
    = AllModel All.Model
    | NewModel New.Model


type Msg
    = AllMsg All.Msg
    | NewMsg New.Msg


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


all =
        Page.recipe All.page
        { toModel = AllModel
        , toMsg = AllMsg
        , map = Element.map
        }


new =
        Page.recipe New.page
        { toModel = NewModel
        , toMsg = NewMsg
        , map = Element.map
        }


init route_ =
    case route_ of
        Route.All route ->
            all.init route

        Route.New route ->
            new.init route


update msg_ model_ =
    case ( msg_, model_ ) of
        ( AllMsg msg, AllModel model ) ->
            all.update msg model

        ( NewMsg msg, NewModel model ) ->
            new.update msg model

        _ ->
            Page.keep model_


bundle model_ =
    case model_ of
        AllModel model ->
            all.bundle model

        NewModel model ->
            new.bundle model

