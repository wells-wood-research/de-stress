module Pages.NotFound exposing
    ( Model
    , Msg
    , page
    )

import Application.Page as Page
import Element exposing (..)


type alias Model =
    ()


type alias Msg =
    Never


page =
    Page.static
        { title = "not found | elm-spa"
        , view =
            column []
                [ text "Page not found!"
                , paragraph [] [ text "Back to homepage" ]
                ]
        }
