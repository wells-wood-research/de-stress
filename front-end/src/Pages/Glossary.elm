module Pages.Glossary exposing (Model, Msg, Params, page)

import Element exposing (..)
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import FeatherIcons
import List as List
import Shared.Documentation as Docs
import Shared.Style as Style
import Spa.Document exposing (Document)
import Spa.Page as Page exposing (Page)
import Spa.Url exposing (Url)


type alias Params =
    ()


type alias Model =
    { displaySettings : DisplaySettings
    }


type Msg
    = ToggleSectionVisibility HideableSection


type HideableSection
    = Aggrescan3D
    | BUDE
    | DFIRE2
    | DSSP
    | EvoEF2
    | HydroFit
    | PackDens
    | Rosetta


type alias DisplaySettings =
    { aggrescan3D : Bool
    , bude : Bool
    , dfire2 : Bool
    , dssp : Bool
    , evoef2 : Bool
    , hydroFit : Bool
    , packDens : Bool
    , rosetta : Bool
    }


toggleTable :
    { tableVisible : Bool
    , title : String
    , toggleMsg : Msg
    , tableView : Element Msg
    }
    -> Element Msg
toggleTable { tableVisible, title, toggleMsg, tableView } =
    if tableVisible then
        column
            [ padding 5
            , width fill
            ]
            [ row [ Events.onClick toggleMsg ]
                [ FeatherIcons.chevronsDown
                    |> Style.featherIconToElmUi
                , (text <| title) |> Style.h3
                ]
            , el [ padding 10 ] tableView
            ]

    else
        row [ padding 5, width fill, Events.onClick toggleMsg ]
            [ FeatherIcons.chevronsRight
                |> Style.featherIconToElmUi
            , (text <| title) |> Style.h3
            ]


init : Url Params -> Model
init params =
    { displaySettings =
        { aggrescan3D = False
        , bude = False
        , dfire2 = False
        , dssp = False
        , evoef2 = False
        , hydroFit = False
        , packDens = False
        , rosetta = False
        }
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleSectionVisibility section ->
            let
                displaySettings =
                    model.displaySettings
            in
            { model
                | displaySettings =
                    case section of
                        Aggrescan3D ->
                            { displaySettings
                                | aggrescan3D =
                                    not displaySettings.aggrescan3D
                            }

                        BUDE ->
                            { displaySettings
                                | bude =
                                    not displaySettings.bude
                            }

                        DFIRE2 ->
                            { displaySettings
                                | dfire2 =
                                    not displaySettings.dfire2
                            }

                        DSSP ->
                            { displaySettings
                                | dssp =
                                    not displaySettings.dssp
                            }

                        EvoEF2 ->
                            { displaySettings
                                | evoef2 =
                                    not displaySettings.evoef2
                            }

                        HydroFit ->
                            { displaySettings
                                | hydroFit =
                                    not displaySettings.hydroFit
                            }

                        PackDens ->
                            { displaySettings
                                | packDens =
                                    not displaySettings.packDens
                            }

                        Rosetta ->
                            { displaySettings
                                | rosetta =
                                    not displaySettings.rosetta
                            }
            }


page : Page Params Model Msg
page =
    Page.sandbox
        { init = init
        , update = update
        , view = view
        }



-- VIEW


maybeText : Maybe String -> Element Msg
maybeText value =
    case value of
        Just a ->
            text a

        Nothing ->
            text ""


rowViewSoftware : String -> String -> List String -> Element Msg
rowViewSoftware description convention citationList =
    row [ padding 5, spacing 5, width fill, Border.widthEach { top = 1, bottom = 1, left = 0, right = 0 } ]
        [ el [ width <| fillPortion 1, Font.alignLeft ] <| paragraph [] [ text description ]
        , el [ width <| fillPortion 1, Font.alignLeft ] <| paragraph [] [ text convention ]
        , el [ width <| fillPortion 1, Font.alignLeft ] <|
            column
                []
                [ paragraph
                    []
                    [ maybeText (List.head citationList)
                    ]
                , paragraph
                    []
                    [ maybeText (List.head (List.drop 1 citationList))
                    ]
                ]
        ]


rowViewMetrics : String -> String -> Element Msg
rowViewMetrics metricName metricDesc =
    row [ padding 5, spacing 5, width fill, Border.widthEach { top = 0, bottom = 1, left = 0, right = 0 } ]
        [ el [ width <| fillPortion 1, Font.alignLeft ] <| paragraph [] [ text metricName ]
        , el [ width <| fillPortion 1, Font.alignLeft ] <| paragraph [] [ text metricDesc ]
        ]


aggrescan3DSoftwareTable : Element Msg
aggrescan3DSoftwareTable =
    column
        [ width fill ]
        [ row
            [ padding 5, width fill, Border.widthXY 0 2, Font.bold ]
            [ el [ width <| fillPortion 1, Font.alignLeft ] <| text "Description"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "How to use?"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "Citations"
            ]
        , rowViewSoftware
            "Aggregation Propensity"
            Docs.softwareInfo.aggrescan3D.convention
            Docs.softwareInfo.aggrescan3D.citations
        ]


budeSoftwareTable : Element Msg
budeSoftwareTable =
    column
        [ width fill ]
        [ row
            [ padding 5, width fill, Border.widthXY 0 2, Font.bold ]
            [ el [ width <| fillPortion 1, Font.alignLeft ] <| text "Description"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "How to use?"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "Citations"
            ]
        , rowViewSoftware
            "Energy Function"
            Docs.softwareInfo.bude.convention
            Docs.softwareInfo.bude.citations
        ]


dfire2SoftwareTable : Element Msg
dfire2SoftwareTable =
    column
        [ width fill ]
        [ row
            [ padding 5, width fill, Border.widthXY 0 2, Font.bold ]
            [ el [ width <| fillPortion 1, Font.alignLeft ] <| text "Description"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "How to use?"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "Citations"
            ]
        , rowViewSoftware
            "Energy Function"
            Docs.softwareInfo.dfire2.convention
            Docs.softwareInfo.dfire2.citations
        ]


dsspSoftwareTable : Element Msg
dsspSoftwareTable =
    column
        [ width fill ]
        [ row
            [ padding 5, width fill, Border.widthXY 0 2, Font.bold ]
            [ el [ width <| fillPortion 1, Font.alignLeft ] <| text "Description"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "How to use?"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "Citations"
            ]
        , rowViewSoftware
            "Secondary Structure Assignment"
            Docs.softwareInfo.dssp.convention
            Docs.softwareInfo.dssp.citations
        ]


evoef2SoftwareTable : Element Msg
evoef2SoftwareTable =
    column
        [ width fill ]
        [ row
            [ padding 5, width fill, Border.widthXY 0 2, Font.bold ]
            [ el [ width <| fillPortion 1, Font.alignLeft ] <| text "Description"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "How to use?"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "Citations"
            ]
        , rowViewSoftware
            "Energy Function"
            Docs.softwareInfo.evoef2.convention
            Docs.softwareInfo.evoef2.citations
        ]


hydroFitSoftwareTable : Element Msg
hydroFitSoftwareTable =
    column
        [ width fill ]
        [ row
            [ padding 5, width fill, Border.widthXY 0 2, Font.bold ]
            [ el [ width <| fillPortion 1, Font.alignLeft ] <| text "Description"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "How to use?"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "Citations"
            ]
        , rowViewSoftware
            ""
            Docs.softwareInfo.hydroFit.convention
            Docs.softwareInfo.hydroFit.citations
        ]


packDensSoftwareTable : Element Msg
packDensSoftwareTable =
    column
        [ width fill ]
        [ row
            [ padding 5, width fill, Border.widthXY 0 2, Font.bold ]
            [ el [ width <| fillPortion 1, Font.alignLeft ] <| text "Description"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "How to use?"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "Citations"
            ]
        , rowViewSoftware
            ""
            Docs.softwareInfo.packDens.convention
            Docs.softwareInfo.packDens.citations
        ]


rosettaSoftwareTable : Element Msg
rosettaSoftwareTable =
    column
        [ width fill ]
        [ row
            [ padding 5, width fill, Border.widthXY 0 2, Font.bold ]
            [ el [ width <| fillPortion 1, Font.alignLeft ] <| text "Description"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "How to use?"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "Citations"
            ]
        , rowViewSoftware
            "Energy Function"
            Docs.softwareInfo.rosetta.convention
            Docs.softwareInfo.rosetta.citations
        ]


aggrescan3DMetricTable : Element Msg
aggrescan3DMetricTable =
    column
        [ width fill ]
        [ row
            [ padding 5, width fill, Border.widthXY 0 2, Font.bold ]
            [ el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Name"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Description"
            ]
        , rowViewMetrics
            Docs.metricInfo.aggrescan3D.metricName.totalScore
            Docs.metricInfo.aggrescan3D.metricDesc.totalScore
        , rowViewMetrics
            Docs.metricInfo.aggrescan3D.metricName.averageScore
            Docs.metricInfo.aggrescan3D.metricDesc.averageScore
        , rowViewMetrics
            Docs.metricInfo.aggrescan3D.metricName.minimumScore
            Docs.metricInfo.aggrescan3D.metricDesc.minimumScore
        , rowViewMetrics
            Docs.metricInfo.aggrescan3D.metricName.maximumScore
            Docs.metricInfo.aggrescan3D.metricDesc.maximumScore
        ]


budeMetricTable : Element Msg
budeMetricTable =
    column
        [ width fill ]
        [ row
            [ padding 5, width fill, Border.widthXY 0 2, Font.bold ]
            [ el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Name"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Description"
            ]
        , rowViewMetrics
            Docs.metricInfo.bude.metricName.totalEnergy
            Docs.metricInfo.bude.metricDesc.totalEnergy
        , rowViewMetrics
            Docs.metricInfo.bude.metricName.stericEnergy
            Docs.metricInfo.bude.metricDesc.stericEnergy
        , rowViewMetrics
            Docs.metricInfo.bude.metricName.desolvationEnergy
            Docs.metricInfo.bude.metricDesc.desolvationEnergy
        , rowViewMetrics
            Docs.metricInfo.bude.metricName.chargeEnergy
            Docs.metricInfo.bude.metricDesc.chargeEnergy
        ]


dfire2MetricTable : Element Msg
dfire2MetricTable =
    column
        [ width fill ]
        [ row
            [ padding 5, width fill, Border.widthXY 0 2, Font.bold ]
            [ el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Name"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Description"
            ]
        , rowViewMetrics
            Docs.metricInfo.dfire2.metricName.totalEnergy
            Docs.metricInfo.dfire2.metricDesc.totalEnergy
        ]


dsspMetricTable : Element Msg
dsspMetricTable =
    column
        [ width fill ]
        [ row
            [ padding 5, width fill, Border.widthXY 0 2, Font.bold ]
            [ el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Name"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Description"
            ]
        , rowViewMetrics
            Docs.metricInfo.dssp.metricName.alpha_helix
            Docs.metricInfo.dssp.metricDesc.alpha_helix
        , rowViewMetrics
            Docs.metricInfo.dssp.metricName.beta_bridge
            Docs.metricInfo.dssp.metricDesc.beta_bridge
        , rowViewMetrics
            Docs.metricInfo.dssp.metricName.beta_strand
            Docs.metricInfo.dssp.metricDesc.beta_strand
        , rowViewMetrics
            Docs.metricInfo.dssp.metricName.three_ten_helix
            Docs.metricInfo.dssp.metricDesc.three_ten_helix
        , rowViewMetrics
            Docs.metricInfo.dssp.metricName.pi_helix
            Docs.metricInfo.dssp.metricDesc.pi_helix
        , rowViewMetrics
            Docs.metricInfo.dssp.metricName.turn
            Docs.metricInfo.dssp.metricDesc.turn
        , rowViewMetrics
            Docs.metricInfo.dssp.metricName.bend
            Docs.metricInfo.dssp.metricDesc.bend
        , rowViewMetrics
            Docs.metricInfo.dssp.metricName.loop
            Docs.metricInfo.dssp.metricDesc.loop
        ]


evoef2MetricTable : Element Msg
evoef2MetricTable =
    column
        [ width fill ]
        [ row
            [ padding 5, width fill, Border.widthXY 0 2, Font.bold ]
            [ el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Name"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Description"
            ]
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.totalEnergy
            Docs.metricInfo.evoef2.metricDesc.totalEnergy
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.refTotalEnergy
            Docs.metricInfo.evoef2.metricDesc.refTotalEnergy
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.intraRTotalEnergy
            Docs.metricInfo.evoef2.metricDesc.intraRTotalEnergy
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interSTotalEnergy
            Docs.metricInfo.evoef2.metricDesc.interSTotalEnergy
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interDTotalEnergy
            Docs.metricInfo.evoef2.metricDesc.interDTotalEnergy
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.referenceALA
            Docs.metricInfo.evoef2.metricDesc.referenceALA
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.referenceCYS
            Docs.metricInfo.evoef2.metricDesc.referenceCYS
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.referenceASP
            Docs.metricInfo.evoef2.metricDesc.referenceASP
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.referenceGLU
            Docs.metricInfo.evoef2.metricDesc.referenceGLU
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.referencePHE
            Docs.metricInfo.evoef2.metricDesc.referencePHE
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.referenceGLY
            Docs.metricInfo.evoef2.metricDesc.referenceGLY
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.referenceHIS
            Docs.metricInfo.evoef2.metricDesc.referenceHIS
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.referenceILE
            Docs.metricInfo.evoef2.metricDesc.referenceILE
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.referenceLYS
            Docs.metricInfo.evoef2.metricDesc.referenceLYS
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.referenceLEU
            Docs.metricInfo.evoef2.metricDesc.referenceLEU
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.referenceMET
            Docs.metricInfo.evoef2.metricDesc.referenceMET
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.referenceASN
            Docs.metricInfo.evoef2.metricDesc.referenceASN
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.referencePRO
            Docs.metricInfo.evoef2.metricDesc.referencePRO
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.referenceGLN
            Docs.metricInfo.evoef2.metricDesc.referenceGLN
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.referenceARG
            Docs.metricInfo.evoef2.metricDesc.referenceARG
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.referenceSER
            Docs.metricInfo.evoef2.metricDesc.referenceSER
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.referenceTHR
            Docs.metricInfo.evoef2.metricDesc.referenceTHR
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.referenceVAL
            Docs.metricInfo.evoef2.metricDesc.referenceVAL
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.referenceTRP
            Docs.metricInfo.evoef2.metricDesc.referenceTRP
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.referenceTYR
            Docs.metricInfo.evoef2.metricDesc.referenceTYR
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.intraRVdwatt
            Docs.metricInfo.evoef2.metricDesc.intraRVdwatt
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.intraRVdwrep
            Docs.metricInfo.evoef2.metricDesc.intraRVdwrep
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.intraRElectr
            Docs.metricInfo.evoef2.metricDesc.intraRElectr
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.intraRDeslvP
            Docs.metricInfo.evoef2.metricDesc.intraRDeslvP
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.intraRDeslvH
            Docs.metricInfo.evoef2.metricDesc.intraRDeslvH
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.intraRHbscbbDis
            Docs.metricInfo.evoef2.metricDesc.intraRHbscbbDis
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.intraRHbscbbThe
            Docs.metricInfo.evoef2.metricDesc.intraRHbscbbThe
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.intraRHbscbbPhi
            Docs.metricInfo.evoef2.metricDesc.intraRHbscbbPhi
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.intraRHbscbbPhi
            Docs.metricInfo.evoef2.metricDesc.intraRHbscbbPhi
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.aapropensity
            Docs.metricInfo.evoef2.metricDesc.aapropensity
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.ramachandran
            Docs.metricInfo.evoef2.metricDesc.ramachandran
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.dunbrack
            Docs.metricInfo.evoef2.metricDesc.dunbrack
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interSVdwatt
            Docs.metricInfo.evoef2.metricDesc.interSVdwatt
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interSVdwrep
            Docs.metricInfo.evoef2.metricDesc.interSVdwrep
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interSElectr
            Docs.metricInfo.evoef2.metricDesc.interSElectr
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interSDeslvP
            Docs.metricInfo.evoef2.metricDesc.interSDeslvP
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interSDeslvH
            Docs.metricInfo.evoef2.metricDesc.interSDeslvH
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interSSsbond
            Docs.metricInfo.evoef2.metricDesc.interSSsbond
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interSHbbbbbDis
            Docs.metricInfo.evoef2.metricDesc.interSHbbbbbDis
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interSHbbbbbThe
            Docs.metricInfo.evoef2.metricDesc.interSHbbbbbThe
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interSHbbbbbPhi
            Docs.metricInfo.evoef2.metricDesc.interSHbbbbbPhi
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interSHbscbbDis
            Docs.metricInfo.evoef2.metricDesc.interSHbscbbDis
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interSHbscbbThe
            Docs.metricInfo.evoef2.metricDesc.interSHbscbbThe
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interSHbscbbPhi
            Docs.metricInfo.evoef2.metricDesc.interSHbscbbPhi
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interSHbscscThe
            Docs.metricInfo.evoef2.metricDesc.interSHbscscThe
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interSHbscscPhi
            Docs.metricInfo.evoef2.metricDesc.interSHbscscPhi
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interDVdwatt
            Docs.metricInfo.evoef2.metricDesc.interDVdwatt
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interDVdwrep
            Docs.metricInfo.evoef2.metricDesc.interDVdwrep
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interDElectr
            Docs.metricInfo.evoef2.metricDesc.interDElectr
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interDDeslvP
            Docs.metricInfo.evoef2.metricDesc.interDDeslvP
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interDDeslvH
            Docs.metricInfo.evoef2.metricDesc.interDDeslvH
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interDSsbond
            Docs.metricInfo.evoef2.metricDesc.interDSsbond
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interDHbbbbbDis
            Docs.metricInfo.evoef2.metricDesc.interDHbbbbbDis
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interDHbbbbbThe
            Docs.metricInfo.evoef2.metricDesc.interDHbbbbbThe
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interDHbbbbbPhi
            Docs.metricInfo.evoef2.metricDesc.interDHbbbbbPhi
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interDHbscbbDis
            Docs.metricInfo.evoef2.metricDesc.interDHbscbbDis
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interDHbscbbThe
            Docs.metricInfo.evoef2.metricDesc.interDHbscbbThe
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interDHbscbbPhi
            Docs.metricInfo.evoef2.metricDesc.interDHbscbbPhi
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interDHbscscDis
            Docs.metricInfo.evoef2.metricDesc.interDHbscscDis
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interDHbscscThe
            Docs.metricInfo.evoef2.metricDesc.interDHbscscThe
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interDHbscscPhi
            Docs.metricInfo.evoef2.metricDesc.interDHbscscPhi
        ]


hydroFitMetricTable : Element Msg
hydroFitMetricTable =
    column
        [ width fill ]
        [ row
            [ padding 5, width fill, Border.widthXY 0 2, Font.bold ]
            [ el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Name"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Description"
            ]
        , rowViewMetrics
            Docs.metricInfo.hydroFit.metricName.hydroFit
            Docs.metricInfo.hydroFit.metricDesc.hydroFit
        ]


packDensMetricTable : Element Msg
packDensMetricTable =
    column
        [ width fill ]
        [ row
            [ padding 5, width fill, Border.widthXY 0 2, Font.bold ]
            [ el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Name"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Description"
            ]
        , rowViewMetrics
            Docs.metricInfo.packDens.metricName.packDens
            Docs.metricInfo.packDens.metricDesc.packDens
        ]


rosettaMetricTable : Element Msg
rosettaMetricTable =
    column
        [ width fill ]
        [ row
            [ padding 5, width fill, Border.widthXY 0 2, Font.bold ]
            [ el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Name"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Description"
            ]
        , rowViewMetrics
            Docs.metricInfo.rosetta.metricName.totalEnergy
            Docs.metricInfo.rosetta.metricDesc.totalEnergy
        , rowViewMetrics
            Docs.metricInfo.rosetta.metricName.reference
            Docs.metricInfo.rosetta.metricDesc.reference
        , rowViewMetrics
            Docs.metricInfo.rosetta.metricName.vdwAtt
            Docs.metricInfo.rosetta.metricDesc.vdwAtt
        , rowViewMetrics
            Docs.metricInfo.rosetta.metricName.vdwRep
            Docs.metricInfo.rosetta.metricDesc.vdwRep
        , rowViewMetrics
            Docs.metricInfo.rosetta.metricName.vdwRepIntraR
            Docs.metricInfo.rosetta.metricDesc.vdwRepIntraR
        , rowViewMetrics
            Docs.metricInfo.rosetta.metricName.electrostatics
            Docs.metricInfo.rosetta.metricDesc.electrostatics
        , rowViewMetrics
            Docs.metricInfo.rosetta.metricName.solvIso
            Docs.metricInfo.rosetta.metricDesc.solvIso
        , rowViewMetrics
            Docs.metricInfo.rosetta.metricName.solvAniso
            Docs.metricInfo.rosetta.metricDesc.solvAniso
        , rowViewMetrics
            Docs.metricInfo.rosetta.metricName.solvIsoIntraR
            Docs.metricInfo.rosetta.metricDesc.solvIsoIntraR
        , rowViewMetrics
            Docs.metricInfo.rosetta.metricName.hblrbb
            Docs.metricInfo.rosetta.metricDesc.hblrbb
        , rowViewMetrics
            Docs.metricInfo.rosetta.metricName.hbsrbb
            Docs.metricInfo.rosetta.metricDesc.hbsrbb
        , rowViewMetrics
            Docs.metricInfo.rosetta.metricName.hbbbsc
            Docs.metricInfo.rosetta.metricDesc.hbbbsc
        , rowViewMetrics
            Docs.metricInfo.rosetta.metricName.hbscsc
            Docs.metricInfo.rosetta.metricDesc.hbscsc
        , rowViewMetrics
            Docs.metricInfo.rosetta.metricName.ssBond
            Docs.metricInfo.rosetta.metricDesc.ssBond
        , rowViewMetrics
            Docs.metricInfo.rosetta.metricName.rama
            Docs.metricInfo.rosetta.metricDesc.rama
        , rowViewMetrics
            Docs.metricInfo.rosetta.metricName.aaProp
            Docs.metricInfo.rosetta.metricDesc.aaProp
        , rowViewMetrics
            Docs.metricInfo.rosetta.metricName.dunbrack
            Docs.metricInfo.rosetta.metricDesc.dunbrack
        , rowViewMetrics
            Docs.metricInfo.rosetta.metricName.omegaPen
            Docs.metricInfo.rosetta.metricDesc.omegaPen
        , rowViewMetrics
            Docs.metricInfo.rosetta.metricName.openProPen
            Docs.metricInfo.rosetta.metricDesc.openProPen
        , rowViewMetrics
            Docs.metricInfo.rosetta.metricName.tyroPen
            Docs.metricInfo.rosetta.metricDesc.tyroPen
        ]


view : Model -> Document Msg
view model =
    { title = "Glossary"
    , body =
        [ column
            [ centerX
            , spacing 20
            , Style.pageWidths.singleColumn
            , Font.size 16
            ]
            [ column [ spacing 20 ]
                [ text "Aggrescan3D 2.0"
                    |> Style.h2
                , aggrescan3DSoftwareTable
                , toggleTable
                    { tableVisible = model.displaySettings.aggrescan3D
                    , title = "Metric List"
                    , toggleMsg = ToggleSectionVisibility Aggrescan3D
                    , tableView = aggrescan3DMetricTable
                    }
                ]
            , column [ spacing 20 ]
                [ text "BUDE"
                    |> Style.h2
                , budeSoftwareTable
                , toggleTable
                    { tableVisible = model.displaySettings.bude
                    , title = "Metric List"
                    , toggleMsg = ToggleSectionVisibility BUDE
                    , tableView = budeMetricTable
                    }
                ]
            , column [ spacing 20 ]
                [ text "DFIRE2"
                    |> Style.h2
                , dfire2SoftwareTable
                , toggleTable
                    { tableVisible = model.displaySettings.aggrescan3D
                    , title = "Metric List"
                    , toggleMsg = ToggleSectionVisibility Aggrescan3D
                    , tableView = aggrescan3DMetricTable
                    }
                ]
            , column [ spacing 20 ]
                [ text "DSSP"
                    |> Style.h2
                , dsspSoftwareTable
                , toggleTable
                    { tableVisible = model.displaySettings.aggrescan3D
                    , title = "Metric List"
                    , toggleMsg = ToggleSectionVisibility Aggrescan3D
                    , tableView = aggrescan3DMetricTable
                    }
                ]
            , column [ spacing 20 ]
                [ text "EvoEF2"
                    |> Style.h2
                , evoef2SoftwareTable
                , toggleTable
                    { tableVisible = model.displaySettings.aggrescan3D
                    , title = "Metric List"
                    , toggleMsg = ToggleSectionVisibility Aggrescan3D
                    , tableView = aggrescan3DMetricTable
                    }
                ]
            , column [ spacing 20 ]
                [ text "Hydrophobic Fitness"
                    |> Style.h2
                , hydroFitSoftwareTable
                , toggleTable
                    { tableVisible = model.displaySettings.aggrescan3D
                    , title = "Metric List"
                    , toggleMsg = ToggleSectionVisibility Aggrescan3D
                    , tableView = aggrescan3DMetricTable
                    }
                ]
            , column [ spacing 20 ]
                [ text "Packing Density"
                    |> Style.h2
                , packDensSoftwareTable
                , toggleTable
                    { tableVisible = model.displaySettings.aggrescan3D
                    , title = "Metric List"
                    , toggleMsg = ToggleSectionVisibility Aggrescan3D
                    , tableView = aggrescan3DMetricTable
                    }
                ]
            , column [ spacing 20 ]
                [ text "Rosetta"
                    |> Style.h2
                , rosettaSoftwareTable
                , toggleTable
                    { tableVisible = model.displaySettings.aggrescan3D
                    , title = "Metric List"
                    , toggleMsg = ToggleSectionVisibility Aggrescan3D
                    , tableView = aggrescan3DMetricTable
                    }
                ]
            ]
        ]
    }
