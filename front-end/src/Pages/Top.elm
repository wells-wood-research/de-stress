module Pages.Top exposing (Model, Msg, Params, page)

import Element exposing (..)
import Shared.Style exposing (h1, h2)
import Spa.Document exposing (Document)
import Spa.Page as Page exposing (Page)
import Spa.Url exposing (Url)


type alias Params =
    ()


type alias Model =
    Url Params


type alias Msg =
    Never


page : Page Params Model Msg
page =
    Page.static
        { view = view
        }



-- {{{ VIEW


view : Url Params -> Document Msg
view _ =
    { title = "DESTRESS"
    , body =
        [ column
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
        ]
    }



-- }}}
