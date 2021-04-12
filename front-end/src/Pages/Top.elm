module Pages.Top exposing (Model, Msg, Params, page)

import Element exposing (..)
import Element.Font as Font
import Shared.Style as Style
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
            , Style.pageWidths.singleColumn
            , Font.size 16
            ]
            [ paragraph []
                [ text
                    """Welcome to the DEsigned STRucture Evaluation ServiceS, or
                    DE-STRESS for short! DE-STRESS provides a suite of tools for
                    evaluating protein designs. Our aim is to help make protein design
                    more reliable, by providing tools to help you select the most
                    promising designs to take into the lab.
                    """
                , el [ Font.bold ] <|
                    text "Click \"Designs\" to get started"
                , text "."
                ]
            , text "Your Data"
                |> Style.h3
            , paragraph []
                [ text
                    """For privacy reasons, we do not store any data regarding you or
                    your designs on our server. Your structure files are added directly
                    to an "in memory" job queue so nothing is ever written to disk. All
                    application data is stored locally on your device. You can export
                    all this data to a CSV file using the control panel in the "Designs"
                    page. If you want to be certain we don't have access to your
                    designs, consider hosting a local instance of the web application.
                    Take a look at the README in the source code for information on how
                    to do this.
                    """
                ]
            , text "Links"
                |> Style.h3
            , paragraph []
                [ link Style.linkStyle
                    { url = "https://github.com/wells-wood-research/de-stress"
                    , label = text "Source Code"
                    }
                ]
            , paragraph []
                [ link Style.linkStyle
                    { url = "https://www.wellswoodresearchgroup.com/"
                    , label = text "Wells Wood Research Group Website"
                    }
                ]
            , text "Citing DE-STRESS"
                |> Style.h3
            , paragraph []
                [ text
                    """If you use DE-STRESS, please cite the following article:"""
                ]
            , paragraph []
                [ text
                    """Stam MJ and Wood CW (2021)..."""
                ]
            , text "Usage"
                |> Style.h3
            , paragraph []
                [ text
                    """DE-STRESS is open sourced under a permissive MIT license, but
                    please be aware that the version we host online is for
                    """
                , el [ Font.bold ] <|
                    text "non-commercial purposes only"
                , text
                    """. If you want to use DE-STRESS for commercial purposes, you will
                    need to obtain licenses for dependencies that do not permit
                    commercial usage without a license, such as Rosetta. We have tried
                    to make it as simple a possible to host a local instance of the
                    application, but please get in contact with us if you need help
                    doing this.
                    """
                ]
            , text "Contacting Us"
                |> Style.h3
            , paragraph []
                [ text
                    """If you need to get in contact with us there are a couple of ways
                    you can do that. If you find a bug or would like to request a
                    feature, we'd really appreciate it if you report it
                    """
                , link Style.linkStyle
                    { url = "https://github.com/wells-wood-research/de-stress/issues"
                    , label = text "in our issue tracker"
                    }
                , text
                    """. If you're stuck and need help or have any general feedback,
                    please create a post on our
                    """
                , link Style.linkStyle
                    { url =
                        "https://github.com/wells-wood-research/de-stress/discussions"
                    , label = text "GitHub discussion page"
                    }
                , text
                    """. For any other enquiries, please contact Chris Wood by
                    """
                , link Style.linkStyle
                    { url = "mailto:chris.wood@ed.ac.uk"
                    , label = text "email"
                    }
                , text " or on "
                , link Style.linkStyle
                    { url = "https://twitter.com/ChrisWellsWood"
                    , label = text "Twitter"
                    }
                , text "."
                ]
            ]
        ]
    }



-- }}}
