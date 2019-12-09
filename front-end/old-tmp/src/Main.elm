module Main exposing (main)

import Application
import Element
import Generated.Pages as Pages
import Generated.Route as Route
import Global


main =
    Application.create
        { ui =
            { toHtml = Element.layout []
            , map = Element.map
            }
        , global =
            { init = Global.init
            , update = Global.update
            , subscriptions = Global.subscriptions
            }
        , routing =
            { routes = Route.routes
            , toPath = Route.toPath
            , notFound = Route.NotFound ()
            }
        , page = Pages.page
        }
