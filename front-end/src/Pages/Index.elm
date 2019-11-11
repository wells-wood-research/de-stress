module Pages.Index exposing
    ( Model
    , Msg
    , page
    )

import Application.Page as Page
import Element exposing (..)
import Style exposing (h1, h2)


type alias Model =
    ()


type alias Msg =
    Never


page =
    Page.static
        { title = "DE-STRESS"
        , view = view
        }


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
