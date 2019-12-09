module Pages.Top exposing (Model, Msg, page)

import Element exposing (..)
import Element.Font as Font
import Generated.Params as Params
import Spa.Page
import Style exposing (h1, h2)
import Utils.Spa exposing (Page)


type alias Model =
    ()


type alias Msg =
    Never


page : Page Params.Top Model Msg model msg appMsg
page =
    Spa.Page.static
        { title = always "DESTRESS - Home"
        , view = always view
        }



-- VIEW


view : Element Msg
view =
    column [ spacing 20 ]
        [ text "Homepage"
            |> h1
        , paragraph []
            [ text
                """Welcome to the DEsigned STRucture Evaluation ServiceS, or DE-STRESS
                for short! DE-STRESS provides a suite of tools for evaluating protein
                designs."""
            ]
        , text "Tutorials"
            |> h2
        , paragraph []
            [ text
                """Tutorials are here..."""
            ]
        , text "Source Code"
            |> h2
        , paragraph []
            [ text
                """Link to source..."""
            ]
        , text "References"
            |> h2
        , paragraph []
            [ text
                """Wood CW (2019) ..."""
            ]
        ]
