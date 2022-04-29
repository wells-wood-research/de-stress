module Pages.Top exposing (Model, Msg, Params, page)

import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Html
import Html.Attributes as Hatt
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
                    text "Please read the information below"
                , text " before clicking \"Designs\" to get started."
                ]
            , text "Tutorial"
                |> Style.h3
            , column
                [ spacing 20, width fill ]
                [ Html.iframe
                    [ Hatt.height 400
                    , Hatt.src " https://www.youtube.com/embed/H-RGvzcUY7M"
                    , Hatt.title "YouTube video player"
                    , Hatt.attribute "frameborder" "0"
                    , Hatt.attribute
                        "allow"
                        "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture"
                    , Hatt.attribute "allowfullscreen" ""
                    ]
                    []
                    |> html
                    |> el [ centerX, width fill ]
                , paragraph
                    []
                    [ text
                        """The example structures used in the tutorial can be found """
                    , link Style.linkStyle
                        { url = "https://github.com/wells-wood-research/de-stress/raw/master/tutorial/tutorial-structures.tar.xz"
                        , label = text "here"
                        }
                    , text "."
                    ]
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
            , text "Before you start..."
                |> Style.h3
            , paragraph []
                [ text
                    """The usefulness of the scores presented by DE-STRESS are entirely
                    dependent on the quality of the models that you upload. No energy
                    minimisation, relaxation or model optimisation of any other kind is
                    performed, we simply run the metrics on the models that you provide.
                    If any of the metrics that are returned are particularly high, you
                    may want to consider relaxing your model in some way (using
                    something like the """
                , link Style.linkStyle
                    { url =
                        "https://www.rosettacommons.org/docs/latest/application_documentation/structure_prediction/relax"
                    , label = text "Fast Relax protocol"
                    }
                , text
                    """ in Rosetta or """
                , link Style.linkStyle
                    { url =
                        "http://docs.openmm.org/latest/userguide/application/02_running_sims.html#energy-minimization"
                    , label = text "energy minimisation"
                    }
                , text """ in molecular dynamics). Related to this, after you have
                    shortlisted designs based on the output of DE-STRESS, you should
                    consider more in depth evaluation of models and sequences using
                    methods such as molecular dynamics or protein-folding simulations.
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
            , text "Citing DE-STRESS"
                |> Style.h3
            , paragraph []
                [ text
                    """If you use DE-STRESS, please cite the following article:"""
                ]
            , paragraph []
                [ link Style.linkStyle
                    { url = "https://doi.org/10.1093/protein/gzab029"
                    , label = text """Stam MJ and Wood CW (2021) DE-STRESS: A user-friendly
                    web application for the evaluation of protein designs, Protein
                    Engineering, Design and Selection, 34, gzab029, 2021."""
                    }
                ]
            , paragraph []
                [ text
                    """If you have used any of the metrics included in DE-STRESS, please
                    cite the relevant papers shown below:
                    """
                ]
            , citationsTable
            ]
        ]
    }


type alias Citations =
    { aggrescan3D : Element Msg
    , bude : Element Msg
    , dfire2 : Element Msg
    , dssp : Element Msg
    , evoef2 : Element Msg
    , hydro_fit : Element Msg
    , pack_dens : Element Msg
    , rosetta : Element Msg
    }


citations : Citations
citations =
    { aggrescan3D =
        paragraph
            []
            [ text
                """Kuriata et al. (2019). Aggrescan3D standalone package for
                structure-based prediction of protein aggregation properties.
                Bioinformatics 35, 3834–3835.
                """
            ]
    , bude =
        column []
            [ paragraph
                []
                [ text
                    """McIntosh-Smith et al. (2012). Benchmarking Energy Efficiency,
                    Power Costs and Carbon Emissions on Heterogeneous Systems.  The
                    Computer Journal 55, 192–205.
                    """
                ]
            , paragraph
                []
                [ text
                    """McIntosh-Smith et al. (2015). High performance in silico virtual
                    drug screening on many-core processors.  The International Journal
                    of High Performance Computing Applications 29, 119–134.
                    """
                ]
            ]
    , dfire2 =
        paragraph
            []
            [ text
                """Yang et al. (2008). Ab initio folding of terminal segments with
                secondary structures reveals the fine difference between two closely
                related all-atom statistical energy functions.  Protein Science 17,
                1212–1219.
                """
            ]
    , dssp =
        column
            []
            [ paragraph
                []
                [ text
                    """Kabsch et al. (1983). Dictionary of protein secondary structure:
                    Pattern recognition of hydrogen-bonded and geometrical features.
                    Biopolymers 22, 2577–2637.
                    """
                ]
            , paragraph
                []
                [ text
                    """Touw et al. (2015). A series of PDB-related databanks for
                    everyday needs.  Nucleic Acids Research 43, D364–D368.
                    """
                ]
            ]
    , evoef2 =
        paragraph
            []
            [ text
                """Huang et al. (2020). EvoEF2: accurate and fast energy function for
                computational protein design.  Bioinformatics 36, 1135–1142.
                """
            ]
    , hydro_fit =
        column
            []
            [ paragraph
                []
                [ text
                    """Huang et al. (1995). Recognizing native folds by the arrangement
                    of hydrophobic and polar residues. J Mol Biol 252, 709–720.
                    """
                ]
            , paragraph
                []
                [ text
                    """Wood et al. (2017). ISAMBARD: an open-source computational
                    environment for biomolecular analysis, modelling and design.
                    Bioinformatics 33, 3043–3050.
                    """
                ]
            ]
    , pack_dens =
        column
            []
            [ paragraph
                []
                [ text
                    """Weiss (2007). On the interrelationship between atomic
                    displacement parameters (ADPs) and coordinates in protein
                    structures. Acta Crystallogr D Biol Crystallogr 63, 1235–1242.
                    """
                ]
            , paragraph
                []
                [ text
                    """Wood et al. (2017). ISAMBARD: an open-source computational
                    environment for biomolecular analysis, modelling and design.
                    Bioinformatics 33, 3043–3050.
                    """
                ]
            ]
    , rosetta =
        paragraph
            []
            [ text
                """Alford et al. (2017). The Rosetta All-Atom Energy Function for
                Macromolecular Modeling and Design.  J. Chem. Theory Comput. 13,
                3031–3048.
                """
            ]
    }


citationsTable : Element Msg
citationsTable =
    column
        [ width fill ]
        [ row
            [ padding 5, width fill, Border.widthXY 0 2, Font.bold ]
            [ el [ width <| fillPortion 1 ] <| text "External Software"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "Citations"
            ]
        , rowViewCitations
            "Aggrescan3D"
            citations.aggrescan3D
        , rowViewCitations
            "BUDE"
            citations.bude
        , rowViewCitations
            "DFIRE2"
            citations.dfire2
        , rowViewCitations
            "DSSP"
            citations.dssp
        , rowViewCitations
            "EvoEF2"
            citations.evoef2
        , rowViewCitations
            "Hydrophobic Fitness"
            citations.hydro_fit
        , rowViewCitations
            "Packing Density"
            citations.pack_dens
        , rowViewCitations
            "Rosetta"
            citations.rosetta
        ]


rowViewCitations : String -> Element Msg -> Element Msg
rowViewCitations softwareName citation =
    row [ padding 5, spacing 5, width fill, Border.widthEach { top = 0, bottom = 1, left = 0, right = 0 } ]
        [ paragraph [ width <| fillPortion 1 ] [ paragraph [] [ text softwareName ] ]
        , el [ width <| fillPortion 3, Font.alignLeft ] <| paragraph [] [ citation ]
        ]



-- }}}
