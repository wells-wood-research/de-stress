module Pages.Top exposing (Model, Msg, page)

import Element exposing (..)
import Generated.Params as Params
import Shared.Style exposing (h1, h2)
import Spa.Page
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
    column
        [ centerX
        , spacing 20
        , width <| maximum 800 <| fill
        ]
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
