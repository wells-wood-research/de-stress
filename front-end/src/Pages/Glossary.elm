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


rowViewSoftware : String -> String -> String -> String -> List String -> Element Msg
rowViewSoftware softwareName description convention commandUsed citationList =
    row [ padding 5, spacing 5, width fill, Border.widthEach { top = 1, bottom = 1, left = 0, right = 0 } ]
        [ el [ width <| fillPortion 1, Font.alignLeft ] <| text softwareName
        , el [ width <| fillPortion 1, Font.alignLeft ] <| paragraph [] [ text description ]
        , el [ width <| fillPortion 1, Font.alignLeft ] <| paragraph [] [ text convention ]
        , el [ width <| fillPortion 1, Font.alignLeft ] <| paragraph [] [ text commandUsed ]
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


rowViewMetrics : String -> String -> String -> Element Msg
rowViewMetrics metricName metricDesc metricVarName =
    row [ padding 5, spacing 5, width fill, Border.widthEach { top = 0, bottom = 1, left = 0, right = 0 } ]
        [ el [ width <| fillPortion 1, Font.alignLeft ] <| paragraph [] [ text metricName ]
        , el [ width <| fillPortion 1, Font.alignLeft ] <| paragraph [] [ text metricDesc ]
        , el [ width <| fillPortion 1, Font.alignLeft ] <| paragraph [] [ text metricVarName ]
        ]


softwareTable : Element Msg
softwareTable =
    column
        [ width fill ]
        [ row
            [ padding 5, width fill, Border.widthXY 0 2, Font.bold ]
            [ el [ width <| fillPortion 1, Font.alignLeft ] <| text "Software Name"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "Description"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "Convention for Use"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "Command Used"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "Citations"
            ]
        , column []
            [ aggrescan3DSoftwareTable
            , budeSoftwareTable
            , dfire2SoftwareTable
            , dsspSoftwareTable
            , evoef2SoftwareTable
            , hydroFitSoftwareTable
            , packDensSoftwareTable
            , rosettaSoftwareTable
            ]
        ]


aggrescan3DSoftwareTable : Element Msg
aggrescan3DSoftwareTable =
    column
        [ width fill ]
        [ rowViewSoftware
            "Aggrescan3D 2.0"
            "Aggregation Propensity"
            Docs.softwareInfo.aggrescan3D.convention
            ""
            Docs.softwareInfo.aggrescan3D.citations
        ]


budeSoftwareTable : Element Msg
budeSoftwareTable =
    column
        [ width fill ]
        [ rowViewSoftware
            "BUDE"
            "Energy Function"
            Docs.softwareInfo.bude.convention
            ""
            Docs.softwareInfo.bude.citations
        ]


dfire2SoftwareTable : Element Msg
dfire2SoftwareTable =
    column
        [ width fill ]
        [ rowViewSoftware
            "DFIRE2"
            "Energy Function"
            Docs.softwareInfo.dfire2.convention
            ""
            Docs.softwareInfo.dfire2.citations
        ]


dsspSoftwareTable : Element Msg
dsspSoftwareTable =
    column
        [ width fill ]
        [ rowViewSoftware
            "DSSP"
            "Secondary Structure Assignment"
            Docs.softwareInfo.dssp.convention
            ""
            Docs.softwareInfo.dssp.citations
        ]


evoef2SoftwareTable : Element Msg
evoef2SoftwareTable =
    column
        [ width fill ]
        [ rowViewSoftware
            "EvoEF2"
            "Energy Function"
            Docs.softwareInfo.evoef2.convention
            ""
            Docs.softwareInfo.evoef2.citations
        ]


hydroFitSoftwareTable : Element Msg
hydroFitSoftwareTable =
    column
        [ width fill ]
        [ rowViewSoftware
            "Hydrophobic Fitness"
            ""
            Docs.softwareInfo.hydroFit.convention
            ""
            Docs.softwareInfo.hydroFit.citations
        ]


packDensSoftwareTable : Element Msg
packDensSoftwareTable =
    column
        [ width fill ]
        [ rowViewSoftware
            "Packing Density"
            ""
            Docs.softwareInfo.packDens.convention
            ""
            Docs.softwareInfo.packDens.citations
        ]


rosettaSoftwareTable : Element Msg
rosettaSoftwareTable =
    column
        [ width fill ]
        [ rowViewSoftware
            "Rosetta"
            "Energy Function"
            Docs.softwareInfo.rosetta.convention
            ""
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
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "Variable Name in CSV Output"
            ]
        , rowViewMetrics
            Docs.metricInfo.aggrescan3D.metricName.totalScore
            Docs.metricInfo.aggrescan3D.metricDesc.totalScore
            Docs.metricInfo.aggrescan3D.metricVarName.totalScore
        , rowViewMetrics
            Docs.metricInfo.aggrescan3D.metricName.averageScore
            Docs.metricInfo.aggrescan3D.metricDesc.averageScore
            Docs.metricInfo.aggrescan3D.metricVarName.averageScore
        , rowViewMetrics
            Docs.metricInfo.aggrescan3D.metricName.minimumScore
            Docs.metricInfo.aggrescan3D.metricDesc.minimumScore
            Docs.metricInfo.aggrescan3D.metricVarName.minimumScore
        , rowViewMetrics
            Docs.metricInfo.aggrescan3D.metricName.maximumScore
            Docs.metricInfo.aggrescan3D.metricDesc.maximumScore
            Docs.metricInfo.aggrescan3D.metricVarName.maximumScore
        ]


budeMetricTable : Element Msg
budeMetricTable =
    column
        [ width fill ]
        [ row
            [ padding 5, width fill, Border.widthXY 0 2, Font.bold ]
            [ el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Name"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Description"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "Variable Name in CSV Output"
            ]
        , rowViewMetrics
            Docs.metricInfo.bude.metricName.totalEnergy
            Docs.metricInfo.bude.metricDesc.totalEnergy
            Docs.metricInfo.bude.metricVarName.totalEnergy
        , rowViewMetrics
            Docs.metricInfo.bude.metricName.stericEnergy
            Docs.metricInfo.bude.metricDesc.stericEnergy
            Docs.metricInfo.bude.metricVarName.stericEnergy
        , rowViewMetrics
            Docs.metricInfo.bude.metricName.desolvationEnergy
            Docs.metricInfo.bude.metricDesc.desolvationEnergy
            Docs.metricInfo.bude.metricVarName.desolvationEnergy
        , rowViewMetrics
            Docs.metricInfo.bude.metricName.chargeEnergy
            Docs.metricInfo.bude.metricDesc.chargeEnergy
            Docs.metricInfo.bude.metricVarName.chargeEnergy
        ]


dfire2MetricTable : Element Msg
dfire2MetricTable =
    column
        [ width fill ]
        [ row
            [ padding 5, width fill, Border.widthXY 0 2, Font.bold ]
            [ el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Name"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Description"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "Variable Name in CSV Output"
            ]
        , rowViewMetrics
            Docs.metricInfo.dfire2.metricName.totalEnergy
            Docs.metricInfo.dfire2.metricDesc.totalEnergy
            Docs.metricInfo.dfire2.metricVarName.totalEnergy
        ]


dsspMetricTable : Element Msg
dsspMetricTable =
    column
        [ width fill ]
        [ row
            [ padding 5, width fill, Border.widthXY 0 2, Font.bold ]
            [ el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Name"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Description"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "Variable Name in CSV Output"
            ]
        , rowViewMetrics
            Docs.metricInfo.dssp.metricName.alpha_helix
            Docs.metricInfo.dssp.metricDesc.alpha_helix
            Docs.metricInfo.dssp.metricVarName.alpha_helix
        , rowViewMetrics
            Docs.metricInfo.dssp.metricName.beta_bridge
            Docs.metricInfo.dssp.metricDesc.beta_bridge
            Docs.metricInfo.dssp.metricVarName.beta_bridge
        , rowViewMetrics
            Docs.metricInfo.dssp.metricName.beta_strand
            Docs.metricInfo.dssp.metricDesc.beta_strand
            Docs.metricInfo.dssp.metricVarName.beta_strand
        , rowViewMetrics
            Docs.metricInfo.dssp.metricName.three_ten_helix
            Docs.metricInfo.dssp.metricDesc.three_ten_helix
            Docs.metricInfo.dssp.metricVarName.three_ten_helix
        , rowViewMetrics
            Docs.metricInfo.dssp.metricName.pi_helix
            Docs.metricInfo.dssp.metricDesc.pi_helix
            Docs.metricInfo.dssp.metricVarName.pi_helix
        , rowViewMetrics
            Docs.metricInfo.dssp.metricName.turn
            Docs.metricInfo.dssp.metricDesc.turn
            Docs.metricInfo.dssp.metricVarName.turn
        , rowViewMetrics
            Docs.metricInfo.dssp.metricName.bend
            Docs.metricInfo.dssp.metricDesc.bend
            Docs.metricInfo.dssp.metricVarName.bend
        , rowViewMetrics
            Docs.metricInfo.dssp.metricName.loop
            Docs.metricInfo.dssp.metricDesc.loop
            Docs.metricInfo.dssp.metricVarName.loop
        ]


evoef2MetricTable : Element Msg
evoef2MetricTable =
    column
        [ width fill ]
        [ row
            [ padding 5, width fill, Border.widthXY 0 2, Font.bold ]
            [ el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Name"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Description"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "Variable Name in CSV Output"
            ]
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.totalEnergy
            Docs.metricInfo.evoef2.metricDesc.totalEnergy
            Docs.metricInfo.evoef2.metricVarName.totalEnergy
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.refTotalEnergy
            Docs.metricInfo.evoef2.metricDesc.refTotalEnergy
            Docs.metricInfo.evoef2.metricVarName.refTotalEnergy
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.intraRTotalEnergy
            Docs.metricInfo.evoef2.metricDesc.intraRTotalEnergy
            Docs.metricInfo.evoef2.metricVarName.intraRTotalEnergy
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interSTotalEnergy
            Docs.metricInfo.evoef2.metricDesc.interSTotalEnergy
            Docs.metricInfo.evoef2.metricVarName.interSTotalEnergy
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interDTotalEnergy
            Docs.metricInfo.evoef2.metricDesc.interDTotalEnergy
            Docs.metricInfo.evoef2.metricVarName.interDTotalEnergy
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.referenceALA
            Docs.metricInfo.evoef2.metricDesc.referenceALA
            Docs.metricInfo.evoef2.metricVarName.referenceALA
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.referenceCYS
            Docs.metricInfo.evoef2.metricDesc.referenceCYS
            Docs.metricInfo.evoef2.metricVarName.referenceCYS
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.referenceASP
            Docs.metricInfo.evoef2.metricDesc.referenceASP
            Docs.metricInfo.evoef2.metricVarName.referenceASP
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.referenceGLU
            Docs.metricInfo.evoef2.metricDesc.referenceGLU
            Docs.metricInfo.evoef2.metricVarName.referenceGLU
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.referencePHE
            Docs.metricInfo.evoef2.metricDesc.referencePHE
            Docs.metricInfo.evoef2.metricVarName.referencePHE
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.referenceGLY
            Docs.metricInfo.evoef2.metricDesc.referenceGLY
            Docs.metricInfo.evoef2.metricVarName.referenceGLY
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.referenceHIS
            Docs.metricInfo.evoef2.metricDesc.referenceHIS
            Docs.metricInfo.evoef2.metricVarName.referenceHIS
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.referenceILE
            Docs.metricInfo.evoef2.metricDesc.referenceILE
            Docs.metricInfo.evoef2.metricVarName.referenceILE
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.referenceLYS
            Docs.metricInfo.evoef2.metricDesc.referenceLYS
            Docs.metricInfo.evoef2.metricVarName.referenceLYS
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.referenceLEU
            Docs.metricInfo.evoef2.metricDesc.referenceLEU
            Docs.metricInfo.evoef2.metricVarName.referenceLEU
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.referenceMET
            Docs.metricInfo.evoef2.metricDesc.referenceMET
            Docs.metricInfo.evoef2.metricVarName.referenceMET
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.referenceASN
            Docs.metricInfo.evoef2.metricDesc.referenceASN
            Docs.metricInfo.evoef2.metricVarName.referenceASN
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.referencePRO
            Docs.metricInfo.evoef2.metricDesc.referencePRO
            Docs.metricInfo.evoef2.metricVarName.referencePRO
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.referenceGLN
            Docs.metricInfo.evoef2.metricDesc.referenceGLN
            Docs.metricInfo.evoef2.metricVarName.referenceGLN
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.referenceARG
            Docs.metricInfo.evoef2.metricDesc.referenceARG
            Docs.metricInfo.evoef2.metricVarName.referenceARG
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.referenceSER
            Docs.metricInfo.evoef2.metricDesc.referenceSER
            Docs.metricInfo.evoef2.metricVarName.referenceSER
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.referenceTHR
            Docs.metricInfo.evoef2.metricDesc.referenceTHR
            Docs.metricInfo.evoef2.metricVarName.referenceTHR
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.referenceVAL
            Docs.metricInfo.evoef2.metricDesc.referenceVAL
            Docs.metricInfo.evoef2.metricVarName.referenceVAL
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.referenceTRP
            Docs.metricInfo.evoef2.metricDesc.referenceTRP
            Docs.metricInfo.evoef2.metricVarName.referenceTRP
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.referenceTYR
            Docs.metricInfo.evoef2.metricDesc.referenceTYR
            Docs.metricInfo.evoef2.metricVarName.referenceTYR
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.intraRVdwatt
            Docs.metricInfo.evoef2.metricDesc.intraRVdwatt
            Docs.metricInfo.evoef2.metricVarName.intraRVdwatt
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.intraRVdwrep
            Docs.metricInfo.evoef2.metricDesc.intraRVdwrep
            Docs.metricInfo.evoef2.metricVarName.intraRVdwrep
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.intraRElectr
            Docs.metricInfo.evoef2.metricDesc.intraRElectr
            Docs.metricInfo.evoef2.metricVarName.intraRElectr
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.intraRDeslvP
            Docs.metricInfo.evoef2.metricDesc.intraRDeslvP
            Docs.metricInfo.evoef2.metricVarName.intraRDeslvP
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.intraRDeslvH
            Docs.metricInfo.evoef2.metricDesc.intraRDeslvH
            Docs.metricInfo.evoef2.metricVarName.intraRDeslvH
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.intraRHbscbbDis
            Docs.metricInfo.evoef2.metricDesc.intraRHbscbbDis
            Docs.metricInfo.evoef2.metricVarName.intraRHbscbbDis
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.intraRHbscbbThe
            Docs.metricInfo.evoef2.metricDesc.intraRHbscbbThe
            Docs.metricInfo.evoef2.metricVarName.intraRHbscbbThe
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.intraRHbscbbPhi
            Docs.metricInfo.evoef2.metricDesc.intraRHbscbbPhi
            Docs.metricInfo.evoef2.metricVarName.intraRHbscbbPhi
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.aapropensity
            Docs.metricInfo.evoef2.metricDesc.aapropensity
            Docs.metricInfo.evoef2.metricVarName.aapropensity
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.ramachandran
            Docs.metricInfo.evoef2.metricDesc.ramachandran
            Docs.metricInfo.evoef2.metricVarName.ramachandran
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.dunbrack
            Docs.metricInfo.evoef2.metricDesc.dunbrack
            Docs.metricInfo.evoef2.metricVarName.dunbrack
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interSVdwatt
            Docs.metricInfo.evoef2.metricDesc.interSVdwatt
            Docs.metricInfo.evoef2.metricVarName.interSVdwatt
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interSVdwrep
            Docs.metricInfo.evoef2.metricDesc.interSVdwrep
            Docs.metricInfo.evoef2.metricVarName.interSVdwrep
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interSElectr
            Docs.metricInfo.evoef2.metricDesc.interSElectr
            Docs.metricInfo.evoef2.metricVarName.interSElectr
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interSDeslvP
            Docs.metricInfo.evoef2.metricDesc.interSDeslvP
            Docs.metricInfo.evoef2.metricVarName.interSDeslvP
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interSDeslvH
            Docs.metricInfo.evoef2.metricDesc.interSDeslvH
            Docs.metricInfo.evoef2.metricVarName.interSDeslvH
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interSSsbond
            Docs.metricInfo.evoef2.metricDesc.interSSsbond
            Docs.metricInfo.evoef2.metricVarName.interSSsbond
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interSHbbbbbDis
            Docs.metricInfo.evoef2.metricDesc.interSHbbbbbDis
            Docs.metricInfo.evoef2.metricVarName.interSHbbbbbDis
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interSHbbbbbThe
            Docs.metricInfo.evoef2.metricDesc.interSHbbbbbThe
            Docs.metricInfo.evoef2.metricVarName.interSHbbbbbThe
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interSHbbbbbPhi
            Docs.metricInfo.evoef2.metricDesc.interSHbbbbbPhi
            Docs.metricInfo.evoef2.metricVarName.interSHbbbbbPhi
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interSHbscbbDis
            Docs.metricInfo.evoef2.metricDesc.interSHbscbbDis
            Docs.metricInfo.evoef2.metricVarName.interSHbscbbDis
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interSHbscbbThe
            Docs.metricInfo.evoef2.metricDesc.interSHbscbbThe
            Docs.metricInfo.evoef2.metricVarName.interSHbscbbThe
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interSHbscbbPhi
            Docs.metricInfo.evoef2.metricDesc.interSHbscbbPhi
            Docs.metricInfo.evoef2.metricVarName.interSHbscbbPhi
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interSHbscscDis
            Docs.metricInfo.evoef2.metricDesc.interSHbscscDis
            Docs.metricInfo.evoef2.metricVarName.interSHbscscDis
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interSHbscscThe
            Docs.metricInfo.evoef2.metricDesc.interSHbscscThe
            Docs.metricInfo.evoef2.metricVarName.interSHbscscThe
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interSHbscscPhi
            Docs.metricInfo.evoef2.metricDesc.interSHbscscPhi
            Docs.metricInfo.evoef2.metricVarName.interSHbscscPhi
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interDVdwatt
            Docs.metricInfo.evoef2.metricDesc.interDVdwatt
            Docs.metricInfo.evoef2.metricVarName.interDVdwatt
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interDVdwrep
            Docs.metricInfo.evoef2.metricDesc.interDVdwrep
            Docs.metricInfo.evoef2.metricVarName.interDVdwrep
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interDElectr
            Docs.metricInfo.evoef2.metricDesc.interDElectr
            Docs.metricInfo.evoef2.metricVarName.interDElectr
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interDDeslvP
            Docs.metricInfo.evoef2.metricDesc.interDDeslvP
            Docs.metricInfo.evoef2.metricVarName.interDDeslvP
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interDDeslvH
            Docs.metricInfo.evoef2.metricDesc.interDDeslvH
            Docs.metricInfo.evoef2.metricVarName.interDDeslvH
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interDSsbond
            Docs.metricInfo.evoef2.metricDesc.interDSsbond
            Docs.metricInfo.evoef2.metricVarName.interDSsbond
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interDHbbbbbDis
            Docs.metricInfo.evoef2.metricDesc.interDHbbbbbDis
            Docs.metricInfo.evoef2.metricVarName.interDHbbbbbDis
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interDHbbbbbThe
            Docs.metricInfo.evoef2.metricDesc.interDHbbbbbThe
            Docs.metricInfo.evoef2.metricVarName.interDHbbbbbThe
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interDHbbbbbPhi
            Docs.metricInfo.evoef2.metricDesc.interDHbbbbbPhi
            Docs.metricInfo.evoef2.metricVarName.interDHbbbbbPhi
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interDHbscbbDis
            Docs.metricInfo.evoef2.metricDesc.interDHbscbbDis
            Docs.metricInfo.evoef2.metricVarName.interDHbscbbDis
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interDHbscbbThe
            Docs.metricInfo.evoef2.metricDesc.interDHbscbbThe
            Docs.metricInfo.evoef2.metricVarName.interDHbscbbThe
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interDHbscbbPhi
            Docs.metricInfo.evoef2.metricDesc.interDHbscbbPhi
            Docs.metricInfo.evoef2.metricVarName.interDHbscbbPhi
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interDHbscscDis
            Docs.metricInfo.evoef2.metricDesc.interDHbscscDis
            Docs.metricInfo.evoef2.metricVarName.interDHbscscDis
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interDHbscscThe
            Docs.metricInfo.evoef2.metricDesc.interDHbscscThe
            Docs.metricInfo.evoef2.metricVarName.interDHbscscThe
        , rowViewMetrics
            Docs.metricInfo.evoef2.metricName.interDHbscscPhi
            Docs.metricInfo.evoef2.metricDesc.interDHbscscPhi
            Docs.metricInfo.evoef2.metricVarName.interDHbscscPhi
        ]


hydroFitMetricTable : Element Msg
hydroFitMetricTable =
    column
        [ width fill ]
        [ row
            [ padding 5, width fill, Border.widthXY 0 2, Font.bold ]
            [ el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Name"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Description"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "Variable Name in CSV Output"
            ]
        , rowViewMetrics
            Docs.metricInfo.hydroFit.metricName.hydroFit
            Docs.metricInfo.hydroFit.metricDesc.hydroFit
            Docs.metricInfo.hydroFit.metricVarName.hydroFit
        ]


packDensMetricTable : Element Msg
packDensMetricTable =
    column
        [ width fill ]
        [ row
            [ padding 5, width fill, Border.widthXY 0 2, Font.bold ]
            [ el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Name"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Description"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "Variable Name in CSV Output"
            ]
        , rowViewMetrics
            Docs.metricInfo.packDens.metricName.packDens
            Docs.metricInfo.packDens.metricDesc.packDens
            Docs.metricInfo.packDens.metricVarName.packDens
        ]


rosettaMetricTable : Element Msg
rosettaMetricTable =
    column
        [ width fill ]
        [ row
            [ padding 5, width fill, Border.widthXY 0 2, Font.bold ]
            [ el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Name"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Description"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "Variable Name in CSV Output"
            ]
        , rowViewMetrics
            Docs.metricInfo.rosetta.metricName.totalEnergy
            Docs.metricInfo.rosetta.metricDesc.totalEnergy
            Docs.metricInfo.rosetta.metricVarName.totalEnergy
        , rowViewMetrics
            Docs.metricInfo.rosetta.metricName.reference
            Docs.metricInfo.rosetta.metricDesc.reference
            Docs.metricInfo.rosetta.metricVarName.reference
        , rowViewMetrics
            Docs.metricInfo.rosetta.metricName.vdwAtt
            Docs.metricInfo.rosetta.metricDesc.vdwAtt
            Docs.metricInfo.rosetta.metricVarName.vdwAtt
        , rowViewMetrics
            Docs.metricInfo.rosetta.metricName.vdwRep
            Docs.metricInfo.rosetta.metricDesc.vdwRep
            Docs.metricInfo.rosetta.metricVarName.vdwRep
        , rowViewMetrics
            Docs.metricInfo.rosetta.metricName.vdwRepIntraR
            Docs.metricInfo.rosetta.metricDesc.vdwRepIntraR
            Docs.metricInfo.rosetta.metricVarName.vdwRepIntraR
        , rowViewMetrics
            Docs.metricInfo.rosetta.metricName.electrostatics
            Docs.metricInfo.rosetta.metricDesc.electrostatics
            Docs.metricInfo.rosetta.metricVarName.electrostatics
        , rowViewMetrics
            Docs.metricInfo.rosetta.metricName.solvIso
            Docs.metricInfo.rosetta.metricDesc.solvIso
            Docs.metricInfo.rosetta.metricVarName.solvIso
        , rowViewMetrics
            Docs.metricInfo.rosetta.metricName.solvAniso
            Docs.metricInfo.rosetta.metricDesc.solvAniso
            Docs.metricInfo.rosetta.metricVarName.solvAniso
        , rowViewMetrics
            Docs.metricInfo.rosetta.metricName.solvIsoIntraR
            Docs.metricInfo.rosetta.metricDesc.solvIsoIntraR
            Docs.metricInfo.rosetta.metricVarName.solvIsoIntraR
        , rowViewMetrics
            Docs.metricInfo.rosetta.metricName.hblrbb
            Docs.metricInfo.rosetta.metricDesc.hblrbb
            Docs.metricInfo.rosetta.metricVarName.hblrbb
        , rowViewMetrics
            Docs.metricInfo.rosetta.metricName.hbsrbb
            Docs.metricInfo.rosetta.metricDesc.hbsrbb
            Docs.metricInfo.rosetta.metricVarName.hbsrbb
        , rowViewMetrics
            Docs.metricInfo.rosetta.metricName.hbbbsc
            Docs.metricInfo.rosetta.metricDesc.hbbbsc
            Docs.metricInfo.rosetta.metricVarName.hbbbsc
        , rowViewMetrics
            Docs.metricInfo.rosetta.metricName.hbscsc
            Docs.metricInfo.rosetta.metricDesc.hbscsc
            Docs.metricInfo.rosetta.metricVarName.hbscsc
        , rowViewMetrics
            Docs.metricInfo.rosetta.metricName.ssBond
            Docs.metricInfo.rosetta.metricDesc.ssBond
            Docs.metricInfo.rosetta.metricVarName.ssBond
        , rowViewMetrics
            Docs.metricInfo.rosetta.metricName.rama
            Docs.metricInfo.rosetta.metricDesc.rama
            Docs.metricInfo.rosetta.metricVarName.rama
        , rowViewMetrics
            Docs.metricInfo.rosetta.metricName.aaProp
            Docs.metricInfo.rosetta.metricDesc.aaProp
            Docs.metricInfo.rosetta.metricVarName.aaProp
        , rowViewMetrics
            Docs.metricInfo.rosetta.metricName.dunbrack
            Docs.metricInfo.rosetta.metricDesc.dunbrack
            Docs.metricInfo.rosetta.metricVarName.dunbrack
        , rowViewMetrics
            Docs.metricInfo.rosetta.metricName.omegaPen
            Docs.metricInfo.rosetta.metricDesc.omegaPen
            Docs.metricInfo.rosetta.metricVarName.omegaPen
        , rowViewMetrics
            Docs.metricInfo.rosetta.metricName.openProPen
            Docs.metricInfo.rosetta.metricDesc.openProPen
            Docs.metricInfo.rosetta.metricVarName.openProPen
        , rowViewMetrics
            Docs.metricInfo.rosetta.metricName.tyroPen
            Docs.metricInfo.rosetta.metricDesc.tyroPen
            Docs.metricInfo.rosetta.metricVarName.tyroPen
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
                [ paragraph [] [ text """DE-STRESS uses a wide range of metrics that come from 
                                         software developed by researchers all across
                                         the world and are well used in protein design. However, due to the 
                                         large numbers of metrics from different sources, it can be difficult 
                                         to keep track of what these different metrics mean and 
                                         how to use them. This page has been created to help you understand 
                                         what these metrics mean, where they have came from and their convention 
                                         for use.""" ]
                , paragraph [] [ text """The first table gives a description of each of the different software
                                         that are included in DE-STRESS, a short section about the convention on 
                                         using these metrics, the command used by DE-STRESS to obtain the results 
                                         and the citations.""" ]
                , paragraph [] [ text """Below this table there are sections for each of the different software that 
                                      can be expanded to show a list of the different metrics, along with a description 
                                      of what these metrics mean.""" ]
                , softwareTable
                , toggleTable
                    { tableVisible = model.displaySettings.aggrescan3D
                    , title = "Aggrescan3D 2.0 Metric List"
                    , toggleMsg = ToggleSectionVisibility Aggrescan3D
                    , tableView = aggrescan3DMetricTable
                    }
                ]
            , column [ spacing 20 ]
                [ toggleTable
                    { tableVisible = model.displaySettings.bude
                    , title = "BUDE Metric List"
                    , toggleMsg = ToggleSectionVisibility BUDE
                    , tableView = budeMetricTable
                    }
                ]
            , column [ spacing 20 ]
                [ toggleTable
                    { tableVisible = model.displaySettings.dfire2
                    , title = "DFIRE2 Metric List"
                    , toggleMsg = ToggleSectionVisibility DFIRE2
                    , tableView = dfire2MetricTable
                    }
                ]
            , column [ spacing 20 ]
                [ toggleTable
                    { tableVisible = model.displaySettings.dssp
                    , title = "DSSP Metric List"
                    , toggleMsg = ToggleSectionVisibility DSSP
                    , tableView = dsspMetricTable
                    }
                ]
            , column [ spacing 20 ]
                [ toggleTable
                    { tableVisible = model.displaySettings.evoef2
                    , title = "EvoEF2 Metric List"
                    , toggleMsg = ToggleSectionVisibility EvoEF2
                    , tableView = evoef2MetricTable
                    }
                ]
            , column [ spacing 20 ]
                [ toggleTable
                    { tableVisible = model.displaySettings.hydroFit
                    , title = "Hydrophobic Fitness Metric List"
                    , toggleMsg = ToggleSectionVisibility HydroFit
                    , tableView = hydroFitMetricTable
                    }
                ]
            , column [ spacing 20 ]
                [ toggleTable
                    { tableVisible = model.displaySettings.packDens
                    , title = "Packing Density Metric List"
                    , toggleMsg = ToggleSectionVisibility PackDens
                    , tableView = packDensMetricTable
                    }
                ]
            , column [ spacing 20 ]
                [ toggleTable
                    { tableVisible = model.displaySettings.rosetta
                    , title = "Rosetta Metric List"
                    , toggleMsg = ToggleSectionVisibility Rosetta
                    , tableView = rosettaMetricTable
                    }
                ]
            ]
        ]
    }
