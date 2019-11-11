module Layouts.Main exposing (view)

import Components.Navbar
import Element exposing (..)
import Global
import Html exposing (..)
import Html.Attributes as Attr


view :
    { page : Element msg
    , global : Global.Model
    }
    -> Element msg
view { page } =
    column
        [ width fill ]
        [ Components.Navbar.view
        , el
            [ centerX
            , paddingXY 50 30
            , spacing 30
            , width (fill |> maximum 800)
            ]
            page
        ]
