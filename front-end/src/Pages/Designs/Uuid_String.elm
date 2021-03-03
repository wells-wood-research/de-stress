port module Pages.Designs.Uuid_String exposing (Model, Msg, Params, page)

import Codec exposing (Value)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import FeatherIcons
import Html
import Html.Attributes as HAtt
import Round
import Shared
import Shared.Buttons as Buttons
import Shared.Design as Design
import Shared.Editable exposing (Editable(..))
import Shared.Folds as Folds
import Shared.Metrics as Metrics
import Shared.Plots as Plots
import Shared.ReferenceSet as ReferenceSet exposing (ReferenceSet)
import Shared.Requirement as Requirement exposing (Requirement, RequirementData)
import Shared.Specification as Specification exposing (Specification)
import Shared.Stored as Stored exposing (Stored(..))
import Shared.Style as Style
import Shared.WebSockets as WebSockets
import Spa.Document exposing (Document)
import Spa.Generated.Route as Route
import Spa.Page as Page exposing (Page)
import Spa.Url exposing (Url)


page : Page Params Model Msg
page =
    Page.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , save = save
        , load = load
        }



-- {{{ PORTS


port setFocussedDesign :
    ({ uuidString : String, design : Value } -> msg)
    -> Sub msg


port setSelectedSpecDesignDetails :
    ({ uuidString : String, specValue : Value } -> msg)
    -> Sub msg


port setSelectedRefSetDesignDetails :
    ({ uuidString : String, refSetValue : Value } -> msg)
    -> Sub msg



-- }}}
-- {{{ MODEL


type alias Model =
    { designUuid : String
    , mSelectedSpecification : Maybe (Stored Specification)
    , mSelectedReferenceSet : Maybe (Stored ReferenceSet)
    , pageState : PageState
    , evoEF2TableOption : EvoEF2TableOption
    , displaySettings : DisplaySettings
    , hoverInfoOption : HoverInfoOption
    }


type PageState
    = AppNotRunning
    | LoadingNoStub
    | LoadingWithStub Design.DesignStub
    | UnknownDesignUuid
    | Design Design.Design


type EvoEF2TableOption
    = Summary
    | Reference
    | IntraR
    | InterS
    | InterD


type HideableSection
    = EvoEF2LogInfo
    | DFIRE2LogInfo
    | RosettaLogInfo


type alias DisplaySettings =
    { evoEF2LogInfo : Bool
    , dfire2LogInfo : Bool
    , rosettaLogInfo : Bool
    }


type HoverInfoOption
    = EvoEF2Total
    | EvoEF2RefTotal
    | EvoEF2IntaRTotal
    | EvoEF2InterSTotal
    | EvoEF2InterDTotal
    | EvoEF2RefALA
    | EvoEF2RefCYS
    | EvoEF2RefASP
    | EvoEF2RefGLU
    | EvoEF2RefPHE
    | EvoEF2RefGLY
    | EvoEF2RefHIS
    | EvoEF2RefILE
    | EvoEF2RefLYS
    | EvoEF2RefLEU
    | EvoEF2RefMET
    | EvoEF2RefASN
    | EvoEF2RefPRO
    | EvoEF2RefGLN
    | EvoEF2RefARG
    | EvoEF2RefSER
    | EvoEF2RefTHR
    | EvoEF2RefVAL
    | EvoEF2RefTRP
    | EvoEF2RefTYR
    | EvoEF2IntraRVDWAtt
    | EvoEF2IntraRVDWRep
    | EvoEF2IntraRElec
    | EvoEF2IntraRDesolvP
    | EvoEF2IntraRDesolvH
    | EvoEF2AAProp
    | EvoEF2Rama
    | EvoEF2Dunbrack
    | EvoEF2IntraRHBSCBBDis
    | EvoEF2IntraRHBSCBBThe
    | EvoEF2IntraRHBSCBBPhi
    | EvoEF2InterSVDWAtt
    | EvoEF2InterSVDWRep
    | EvoEF2InterSElec
    | EvoEF2InterSDesolvP
    | EvoEF2InterSDesolvH
    | EvoEF2InterSSSbond
    | EvoEF2InterSHBBBBBDis
    | EvoEF2InterSHBBBBBThe
    | EvoEF2InterSHBBBBBPhi
    | EvoEF2InterSHBSCBBDis
    | EvoEF2InterSHBSCBBThe
    | EvoEF2InterSHBSCBBPhi
    | EvoEF2InterSHBSCSCDis
    | EvoEF2InterSHBSCSCThe
    | EvoEF2InterSHBSCSCPhi
    | EvoEF2InterDVDWAtt
    | EvoEF2InterDVDWRep
    | EvoEF2InterDElec
    | EvoEF2InterDDesolvP
    | EvoEF2InterDDesolvH
    | EvoEF2InterDSSbond
    | EvoEF2InterDHBBBBBDis
    | EvoEF2InterDHBBBBBThe
    | EvoEF2InterDHBBBBBPhi
    | EvoEF2InterDHBSCBBDis
    | EvoEF2InterDHBSCBBThe
    | EvoEF2InterDHBSCBBPhi
    | EvoEF2InterDHBSCSCDis
    | EvoEF2InterDHBSCSCThe
    | EvoEF2InterDHBSCSCPhi
    | DFIRE2Total
    | RosettaTotal
    | RosettaReference
    | RosettaVDWAtt
    | RosettaVDWRep
    | RosettaVDWRepIntraR
    | RosettaElec
    | RosettaSolvIso
    | RosettaSolvAniso
    | RosettaSolvIsoIntraR
    | RosettaHBLRBB
    | RosettaHBSRBB
    | RosettaHBBBSC
    | RosettaHBSCSC
    | RosettaSSbond
    | RosettaRama
    | RosettaAAProp
    | RosettaDunbrack
    | RosettaOmegaPen
    | RosettaOpenProPen
    | RosettaTyroPen
    | NoHoverInfo


hideableSectionToString : HideableSection -> String
hideableSectionToString hideableSection =
    case hideableSection of
        EvoEF2LogInfo ->
            "EvoEF2 Log Information"

        DFIRE2LogInfo ->
            "DFIRE2 Log Information"

        RosettaLogInfo ->
            "Rosetta Log Information"


evoEF2TableOptionToString : EvoEF2TableOption -> String
evoEF2TableOptionToString evoEF2TableOption =
    case evoEF2TableOption of
        Summary ->
            "Summary"

        Reference ->
            "Reference"

        IntraR ->
            "Intra Residue"

        InterS ->
            "Inter Residue - Same Chain"

        InterD ->
            "Inter Residue - Different Chains"



-- }}}
-- {{{ INIT


type alias Params =
    { uuid : String }


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared { params } =
    case Shared.getRunState shared of
        Just runState ->
            let
                model =
                    { designUuid = params.uuid
                    , mSelectedSpecification =
                        Maybe.map
                            (\uuid -> Stored.OnDisk { uuidString = uuid })
                            runState.mSelectedSpecification
                    , mSelectedReferenceSet =
                        Maybe.map
                            (\uuid -> Stored.OnDisk { uuidString = uuid })
                            runState.mSelectedReferenceSet
                    , pageState =
                        case
                            Dict.get params.uuid runState.designs
                                |> Maybe.map Design.storedDesignToStub
                        of
                            Just stub ->
                                LoadingWithStub stub

                            Nothing ->
                                LoadingNoStub
                    , evoEF2TableOption = Summary
                    , displaySettings =
                        { evoEF2LogInfo = False
                        , dfire2LogInfo = False
                        , rosettaLogInfo = False
                        }
                    , hoverInfoOption =
                        NoHoverInfo
                    }
            in
            ( model
            , Cmd.batch
                [ Design.getStoredDesign { uuidString = params.uuid }
                , case model.mSelectedSpecification of
                    Just stored ->
                        Specification.getSpecificationForDesignDetails
                            { uuidString = Stored.getUuid stored }

                    Nothing ->
                        Cmd.none
                , case model.mSelectedReferenceSet of
                    Just stored ->
                        ReferenceSet.getReferenceSetForDesignDetails
                            { uuidString = Stored.getUuid stored }

                    Nothing ->
                        Cmd.none
                ]
            )

        Nothing ->
            ( { designUuid = params.uuid
              , mSelectedSpecification = Nothing
              , mSelectedReferenceSet = Nothing
              , pageState = AppNotRunning
              , evoEF2TableOption = Summary
              , displaySettings =
                    { evoEF2LogInfo = False
                    , dfire2LogInfo = False
                    , rosettaLogInfo = False
                    }
              , hoverInfoOption =
                    NoHoverInfo
              }
            , Cmd.none
            )



-- }}}
-- {{{ UPDATE


type Msg
    = SetFocus { uuidString : String, design : Value }
    | SetSpecification { uuidString : String, specValue : Value }
    | SetReferenceSet { uuidString : String, refSetValue : Value }
    | SetEvoEF2TableOption EvoEF2TableOption
    | ToggleSectionVisibility HideableSection
    | ChangeHoverInfo HoverInfoOption


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetFocus { design } ->
            case Codec.decodeValue Design.codec design of
                Ok des ->
                    ( { model | pageState = Design des }
                    , Cmd.batch
                        [ case
                            ( model.mSelectedReferenceSet
                                |> Maybe.andThen Stored.getData
                            , WebSockets.getDesignMetrics des.metricsJobStatus
                            )
                          of
                            ( Just referenceSet, Just metrics ) ->
                                plotCommands metrics referenceSet

                            _ ->
                                Cmd.none
                        ]
                    )

                Err _ ->
                    ( { model | pageState = UnknownDesignUuid }
                    , Cmd.none
                    )

        SetSpecification { specValue } ->
            ( { model
                | mSelectedSpecification =
                    case model.mSelectedSpecification of
                        Just selectedSpec ->
                            case Codec.decodeValue Specification.codec specValue of
                                Ok specification ->
                                    InMemory
                                        { uuidString =
                                            Stored.getUuid selectedSpec
                                        , data = specification
                                        }
                                        |> Just

                                Err _ ->
                                    FailedToLoad
                                        { uuidString =
                                            Stored.getUuid selectedSpec
                                        }
                                        |> Just

                        Nothing ->
                            Debug.log
                                "A specification was set, but I was not expecting one."
                                model.mSelectedSpecification
              }
            , Cmd.none
            )

        SetReferenceSet { refSetValue } ->
            case
                ( model.mSelectedReferenceSet
                , Codec.decodeValue ReferenceSet.codec refSetValue
                )
            of
                ( Just selectedRefSet, Ok referenceSet ) ->
                    ( { model
                        | mSelectedReferenceSet =
                            InMemory
                                { uuidString =
                                    Stored.getUuid selectedRefSet
                                , data = referenceSet
                                }
                                |> Just
                      }
                    , case model.pageState of
                        Design design ->
                            case WebSockets.getDesignMetrics design.metricsJobStatus of
                                Just metrics ->
                                    plotCommands metrics referenceSet

                                Nothing ->
                                    Cmd.none

                        _ ->
                            Cmd.none
                    )

                ( Just selectedRefSet, Err _ ) ->
                    ( { model
                        | mSelectedReferenceSet =
                            FailedToLoad
                                { uuidString =
                                    Stored.getUuid
                                        selectedRefSet
                                }
                                |> Just
                      }
                    , Cmd.none
                    )

                ( Nothing, _ ) ->
                    Debug.log
                        "A reference set was set, but I was not expecting one."
                        ( model, Cmd.none )

        SetEvoEF2TableOption option ->
            ( { model | evoEF2TableOption = option }
            , Cmd.none
            )

        ToggleSectionVisibility section ->
            let
                displaySettings =
                    model.displaySettings
            in
            ( { model
                | displaySettings =
                    case section of
                        EvoEF2LogInfo ->
                            { displaySettings
                                | evoEF2LogInfo =
                                    not displaySettings.evoEF2LogInfo
                            }

                        DFIRE2LogInfo ->
                            { displaySettings
                                | dfire2LogInfo =
                                    not displaySettings.dfire2LogInfo
                            }

                        RosettaLogInfo ->
                            { displaySettings
                                | rosettaLogInfo =
                                    not displaySettings.rosettaLogInfo
                            }
              }
            , Cmd.none
            )

        ChangeHoverInfo option ->
            ( { model | hoverInfoOption = option }
            , Cmd.none
            )


plotCommands : Metrics.DesignMetrics -> ReferenceSet -> Cmd msg
plotCommands metrics referenceSet =
    Cmd.batch
        [ Plots.vegaPlot <|
            { plotId = "composition"
            , spec =
                Metrics.createCompositionSpec
                    (referenceSet
                        |> ReferenceSet.getGenericData
                        |> .aggregateData
                    )
                    metrics
            }
        , Plots.vegaPlot <|
            { plotId = "torsionAngles"
            , spec =
                Metrics.createTorsionAngleSpec
                    metrics
                    (referenceSet
                        |> ReferenceSet.getGenericData
                        |> .metrics
                    )
            }
        , Plots.vegaPlot <|
            { plotId = "metricsHistograms"
            , spec =
                Metrics.createAllHistogramsSpec
                    metrics
                    (referenceSet
                        |> ReferenceSet.getGenericData
                        |> .metrics
                    )
            }
        ]


save : Model -> Shared.Model -> Shared.Model
save model shared =
    shared


load : Shared.Model -> Model -> ( Model, Cmd Msg )
load shared model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ setFocussedDesign SetFocus
        , setSelectedSpecDesignDetails SetSpecification
        , setSelectedRefSetDesignDetails SetReferenceSet
        ]



-- }}}
-- {{{ VIEW


view : Model -> Document Msg
view model =
    { title = "Design Details"
    , body = [ bodyView model ]
    }


sectionColumn : List (Element msg) -> Element msg
sectionColumn =
    column [ spacing 12, width fill ]


bodyView : Model -> Element Msg
bodyView model =
    column [ centerX, width (fill |> maximum 800) ]
        [ el [ centerX ] (Style.h1 <| text "Design Details")
        , case model.pageState of
            AppNotRunning ->
                sectionColumn
                    [ paragraph []
                        [ """An error has occurred while initialising the
                            application. In fact, you should never really be able to see
                            this message as the application should not get this far. Try
                            closing your browser and visiting the site again, or try a
                            different browser.
                            """
                            |> text
                        ]
                    ]

            LoadingNoStub ->
                sectionColumn [ text "Loading..." ]

            LoadingWithStub stub ->
                basicInformation stub

            UnknownDesignUuid ->
                sectionColumn
                    [ paragraph []
                        [ "Failed to load a design with UUID: "
                            |> text
                        , el [ Font.bold ] (text model.designUuid)
                        ]
                    , paragraph []
                        [ """This design no longer exists, you might have deleted
                            it or it may be stored on another computer. DESTRESS does
                            not store your designs on our server by default. If you'd
                            like this behaviour, you can opt in using the settings
                            panel.
                            """
                            |> text
                        ]
                    ]

            Design design ->
                sectionColumn
                    [ designDetailsView
                        model.designUuid
                        (Maybe.andThen Stored.getData model.mSelectedSpecification)
                        (Maybe.andThen Stored.getData model.mSelectedReferenceSet)
                        design
                        model.evoEF2TableOption
                        model.displaySettings
                        model.hoverInfoOption
                    ]
        ]


basicInformation : { stubOrDesign | name : String, fileName : String } -> Element msg
basicInformation { name, fileName } =
    column
        [ width fill
        ]
        [ Style.h2 <| text ("Design Name: " ++ name)
        , Style.h3 <| text ("File: " ++ fileName)
        ]


designDetailsView :
    String
    -> Maybe Specification
    -> Maybe ReferenceSet
    -> Design.Design
    -> EvoEF2TableOption
    -> DisplaySettings
    -> HoverInfoOption
    -> Element Msg
designDetailsView uuidString mSpecification mReferenceSet design evoEF2TableOption displaySettings hoverInfoOption =
    let
        { fileName, deleteStatus, metricsJobStatus } =
            design
    in
    column
        [ spacing 15, width fill ]
    <|
        [ sectionColumn
            [ paragraph []
                [ Style.h1 <| text "Design Details" ]
            , row [ height fill, spacing 10 ]
                (case design.name of
                    NotEditing currentName ->
                        [ paragraph [] [ Style.h2 <| text <| "Name: " ++ currentName ]
                        , el [ centerY ]
                            --, Events.onClick <| ClickedNameEdit ]
                            (FeatherIcons.edit
                                |> FeatherIcons.toHtml []
                                |> html
                            )
                        ]

                    Editing _ mNewName ->
                        -- [ Input.text
                        --     Style.textInputStyle
                        --     { onChange = EditedName
                        --     , text = Maybe.withDefault "" mNewName
                        --     , placeholder = Nothing
                        --     , label =
                        --         Input.labelRight [ centerY ]
                        --             (row [ spacing 5 ]
                        --                 [ Style.conditionalButton
                        --                     { label = text "Ok"
                        --                     , clickMsg = Just ClickedAcceptNameEdit
                        --                     , isActive =
                        --                         case mNewName of
                        --                             Just newName ->
                        --                                 String.isEmpty newName
                        --                                     |> not
                        --                             Nothing ->
                        --                                 False
                        --                     }
                        --                 , Style.alwaysActiveButton
                        --                     { label = text "Cancel"
                        --                     , clickMsg = ClickedCancelNameEdit
                        --                     , pressed = False
                        --                     }
                        --                 ]
                        --             )
                        --     }
                        []
                )
            , paragraph [] [ text ("Structure file: " ++ fileName) ]
            ]
        , row [ spacing 10 ]
            [ Buttons.linkButton
                { label = text "Back"
                , route = Route.Designs
                }

            -- , Buttons.dangerousButton
            --     { label = text "Delete"
            --     , confirmText = "Are you sure you want to delete this design?"
            --     , status = deleteStatus
            --     , dangerousMsg = DeleteFocussedDesign uuidString
            --     }
            ]
        , sectionColumn
            [ Style.h2 <| text "Structure"
            , el [ height <| px 400, width fill, padding 5, Border.width 1 ]
                (Html.node "ngl-viewer"
                    [ HAtt.id "viewer"
                    , HAtt.style "width" "100%"
                    , HAtt.style "height" "100%"
                    , HAtt.attribute "pdb-string" design.pdbString
                    ]
                    []
                    |> html
                )
            ]
        ]
            ++ (case WebSockets.getDesignMetrics metricsJobStatus of
                    Just designMetrics ->
                        [ basicMetrics designMetrics
                        , evoEF2ResultsTableView evoEF2TableOption designMetrics displaySettings hoverInfoOption
                        , dfire2ResultsView designMetrics displaySettings hoverInfoOption
                        , rosettaResultsTableView designMetrics displaySettings hoverInfoOption
                        , case mReferenceSet of
                            Just refSet ->
                                referenceSetComparisonView

                            Nothing ->
                                text "No reference set selected."
                        , case mSpecification of
                            Just specification ->
                                specificationView designMetrics specification

                            Nothing ->
                                text "No specification selected."
                        ]

                    Nothing ->
                        [ WebSockets.metricsJobStatusString metricsJobStatus
                            |> text
                        ]
               )


basicMetrics : Metrics.DesignMetrics -> Element msg
basicMetrics metrics =
    let
        { sequenceInfo } =
            metrics
    in
    sectionColumn
        [ Style.h2 <| text "Basic Metrics"
        , Style.h3 <| text "Sequences and DSSP Assignment"
        , sequenceInfoDictView sequenceInfo
        , metricsOverview metrics
        ]


metricsOverview : Metrics.DesignMetrics -> Element msg
metricsOverview metrics =
    column [ spacing 10, width fill ]
        [ Style.h2 <| text "Metrics"
        , wrappedRow
            [ spacing 5 ]
            [ createTableColumn
                (\mHF ->
                    case mHF of
                        Just hf ->
                            onePlaceFloatText hf

                        Nothing ->
                            text "--"
                )
                metrics.hydrophobicFitness
                "Hydrophobic Fitness"
            , createTableColumn
                onePlaceFloatText
                metrics.isoelectricPoint
                "pI"
            , createTableColumn
                intText
                metrics.numOfResidues
                "# of Residues"
            , createTableColumn
                onePlaceFloatText
                metrics.mass
                "Mass (Da)"
            , createTableColumn
                onePlaceFloatText
                metrics.packingDensity
                "Mean Packing Density"
            ]
        ]


sequenceInfoDictView : Dict String Metrics.SequenceInfo -> Element msg
sequenceInfoDictView sequenceInfoDict =
    sequenceInfoDict
        |> Dict.toList
        |> List.map sequenceInfoView
        |> column [ padding 15, spacing 5, width fill ]


sequenceInfoView : ( String, Metrics.SequenceInfo ) -> Element msg
sequenceInfoView ( chainId, sequenceInfo ) =
    let
        aaView ( aa, dssp ) =
            column []
                [ String.fromChar aa |> text
                , String.fromChar dssp |> text
                ]

        aaList =
            String.toList sequenceInfo.sequence

        dsspList =
            String.toList sequenceInfo.dsspAssignment
                |> List.map
                    (\c ->
                        if c == ' ' then
                            '-'

                        else
                            c
                    )

        zippedSequenceInfo =
            List.map2 Tuple.pair aaList dsspList
    in
    column [ width fill ]
        [ paragraph [ width fill, Font.bold ] [ "Chain: " ++ chainId |> text ]
        , wrappedRow
            [ width fill
            , spacingXY 0 10
            , Font.family
                [ Font.typeface "Roboto Mono"
                , Font.monospace
                ]
            , Font.size 14
            ]
            (List.map aaView zippedSequenceInfo)
        ]


evoEF2ResultsTableView :
    EvoEF2TableOption
    -> Metrics.DesignMetrics
    -> DisplaySettings
    -> HoverInfoOption
    -> Element Msg
evoEF2ResultsTableView evoEF2TableOption metrics displaySettings hoverInfoOption =
    let
        radioInputSelection =
            el
                [ spacing 20
                , padding 20
                , centerX
                ]
            <|
                Input.radio
                    [ padding 20
                    , spacing 20
                    , scrollbarX
                    ]
                    { onChange = SetEvoEF2TableOption
                    , selected = Just evoEF2TableOption
                    , label = Input.labelAbove [] (text "Select the table view for the EvoEF2 results")
                    , options =
                        [ Input.option Summary (text (evoEF2TableOptionToString Summary))
                        , Input.option Reference (text (evoEF2TableOptionToString Reference))
                        , Input.option IntraR (text (evoEF2TableOptionToString IntraR))
                        , Input.option InterS (text (evoEF2TableOptionToString InterS))
                        , Input.option InterD (text (evoEF2TableOptionToString InterD))
                        ]
                    }

        logInfoBox =
            paragraph
                [ spacing 20
                , padding 20
                , width fill
                , Font.family
                    [ Font.typeface "Roboto Mono"
                    , Font.monospace
                    ]
                , Font.size 10
                ]
                [ text metrics.evoEF2Results.log_info
                ]
    in
    sectionColumn
        [ Style.h3 <|
            paragraph []
                [ text
                    ("EvoEF2 Energy Function Results - "
                        ++ evoEF2TableOptionToString evoEF2TableOption
                    )
                ]
        , radioInputSelection
        , wrappedRow
            [ spacing 5 ]
            (case evoEF2TableOption of
                Summary ->
                    evoef2SummaryColumns metrics hoverInfoOption

                Reference ->
                    evoef2RefColumns metrics hoverInfoOption

                IntraR ->
                    evoef2IntraRColumns metrics hoverInfoOption

                InterS ->
                    evoef2InterSColumns metrics hoverInfoOption

                InterD ->
                    evoef2InterDColumns metrics hoverInfoOption
            )
        , Folds.sectionFoldView
            { foldVisible = displaySettings.evoEF2LogInfo
            , title = hideableSectionToString EvoEF2LogInfo
            , toggleMsg = ToggleSectionVisibility EvoEF2LogInfo
            , contentView = logInfoBox
            }
        ]


evoef2SummaryColumns :
    Metrics.DesignMetrics
    -> HoverInfoOption
    -> List (Element Msg)
evoef2SummaryColumns metrics hoverInfoOption =
    let
        evoEF2SummaryTotalHoverBox : List (Attribute Msg)
        evoEF2SummaryTotalHoverBox =
            hoverInfoView
                { title = "Total EvoEF2"
                , info = """This value is the total EvoEF2 energy. It is the sum of the reference, 
                            intra residue, inter residue - same chain and inter residue - different 
                            chains, energy values. In the EvoEF2 output this field is called `Total`."""
                , mouseEnterMsg = EvoEF2Total
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2SummaryRefHoverBox : List (Attribute Msg)
        evoEF2SummaryRefHoverBox =
            hoverInfoView
                { title = "Reference"
                , info = """This value is the total reference energy. This value is not included in 
                            the EvoEF2 output and is calculated in DE-STRESS."""
                , mouseEnterMsg = EvoEF2RefTotal
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2SummaryIntraRHoverBox : List (Attribute Msg)
        evoEF2SummaryIntraRHoverBox =
            hoverInfoView
                { title = "Intra Residues"
                , info = """This value is the total energy for intra residue interactions. This value is 
                            not included in the EvoEF2 output and is calculated in DE-STRESS."""
                , mouseEnterMsg = EvoEF2IntaRTotal
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2SummaryInterSHoverBox : List (Attribute Msg)
        evoEF2SummaryInterSHoverBox =
            hoverInfoView
                { title = "Inter Residues - Same Chain"
                , info = """This value is the total energy for inter residue interactions in the same chain. 
                            This value is not included in the EvoEF2 output and is calculated in DE-STRESS."""
                , mouseEnterMsg = EvoEF2InterSTotal
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2SummaryInterDHoverBox : List (Attribute Msg)
        evoEF2SummaryInterDHoverBox =
            hoverInfoView
                { title = "Inter Residues - Different Chains"
                , info = """This value is the total energy for inter residue interactions in different chains. 
                            This value is not included in the EvoEF2 output and is calculated in DE-STRESS."""
                , mouseEnterMsg = EvoEF2InterDTotal
                , hoverInfoOption = hoverInfoOption
                }
    in
    [ el evoEF2SummaryTotalHoverBox <| createTableFloatColumn metrics.evoEF2Results.total "Total EvoEF2"
    , el evoEF2SummaryRefHoverBox <| createTableFloatColumn metrics.evoEF2Results.ref_total "Reference"
    , el evoEF2SummaryIntraRHoverBox <| createTableFloatColumn metrics.evoEF2Results.intraR_total "Intra Residue"
    , el evoEF2SummaryInterSHoverBox <| createTableFloatColumn metrics.evoEF2Results.interS_total "Inter Residue - Same Chain"
    , el evoEF2SummaryInterDHoverBox <| createTableFloatColumn metrics.evoEF2Results.interD_total "Inter Residue - Different Chains"
    ]


evoef2RefColumns :
    Metrics.DesignMetrics
    -> HoverInfoOption
    -> List (Element Msg)
evoef2RefColumns metrics hoverInfoOption =
    let
        evoEF2RefALAHoverBox : List (Attribute Msg)
        evoEF2RefALAHoverBox =
            hoverInfoView
                { title = "ALA - Reference"
                , info = """This value is reference energy for the amino acid Alanine (ALA). In the EvoEF2
                            output this value is called `reference_ALA`."""
                , mouseEnterMsg = EvoEF2RefALA
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2RefCYSHoverBox : List (Attribute Msg)
        evoEF2RefCYSHoverBox =
            hoverInfoView
                { title = "CYS - Reference"
                , info = """This value is reference energy for the amino acid Cysteine (CYS). In the EvoEF2
                            output this value is called `reference_CYS`."""
                , mouseEnterMsg = EvoEF2RefCYS
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2RefASPHoverBox : List (Attribute Msg)
        evoEF2RefASPHoverBox =
            hoverInfoView
                { title = "ASP - Reference"
                , info = """This value is reference energy for the amino acid Aspartic acid (ASP). In the EvoEF2
                            output this value is called `reference_ASP`."""
                , mouseEnterMsg = EvoEF2RefASP
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2RefGLUHoverBox : List (Attribute Msg)
        evoEF2RefGLUHoverBox =
            hoverInfoView
                { title = "GLU - Reference"
                , info = """This value is reference energy for the amino acid Glutamic acid (GLU). In the EvoEF2
                            output this value is called `reference_GLU`."""
                , mouseEnterMsg = EvoEF2RefGLU
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2RefPHEHoverBox : List (Attribute Msg)
        evoEF2RefPHEHoverBox =
            hoverInfoView
                { title = "PHE - Reference"
                , info = """This value is reference energy for the amino acid Phenylalanine (PHE). In the EvoEF2
                            output this value is called `reference_PHE`."""
                , mouseEnterMsg = EvoEF2RefPHE
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2RefGLYHoverBox : List (Attribute Msg)
        evoEF2RefGLYHoverBox =
            hoverInfoView
                { title = "GLY - Reference"
                , info = """This value is reference energy for the amino acid glycine (GLY). In the EvoEF2
                            output this value is called `reference_GLY`."""
                , mouseEnterMsg = EvoEF2RefGLY
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2RefHISHoverBox : List (Attribute Msg)
        evoEF2RefHISHoverBox =
            hoverInfoView
                { title = "HIS - Reference"
                , info = """This value is reference energy for the amino acid Histidine (HIS). In the EvoEF2
                            output this value is called `reference_HIS`."""
                , mouseEnterMsg = EvoEF2RefHIS
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2RefILEHoverBox : List (Attribute Msg)
        evoEF2RefILEHoverBox =
            hoverInfoView
                { title = "ILE - Reference"
                , info = """This value is reference energy for the amino acid Isoleucine (ILE). In the EvoEF2
                            output this value is called `reference_ILE`."""
                , mouseEnterMsg = EvoEF2RefILE
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2RefLYSHoverBox : List (Attribute Msg)
        evoEF2RefLYSHoverBox =
            hoverInfoView
                { title = "LYS - Reference"
                , info = """This value is reference energy for the amino acid Lysine (LYS). In the EvoEF2
                            output this value is called `reference_LYS`."""
                , mouseEnterMsg = EvoEF2RefLYS
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2RefLEUHoverBox : List (Attribute Msg)
        evoEF2RefLEUHoverBox =
            hoverInfoView
                { title = "LEU - Reference"
                , info = """This value is reference energy for the amino acid Leucine (LEU). In the EvoEF2
                            output this value is called `reference_LEU`."""
                , mouseEnterMsg = EvoEF2RefLEU
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2RefMETHoverBox : List (Attribute Msg)
        evoEF2RefMETHoverBox =
            hoverInfoView
                { title = "MET - Reference"
                , info = """This value is reference energy for the amino acid Methionine (MET). In the EvoEF2
                            output this value is called `reference_MET`."""
                , mouseEnterMsg = EvoEF2RefMET
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2RefASNHoverBox : List (Attribute Msg)
        evoEF2RefASNHoverBox =
            hoverInfoView
                { title = "ASN - Reference"
                , info = """This value is reference energy for the amino acid Asparagine (ASN). In the EvoEF2
                            output this value is called `reference_ASN`."""
                , mouseEnterMsg = EvoEF2RefASN
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2RefPROHoverBox : List (Attribute Msg)
        evoEF2RefPROHoverBox =
            hoverInfoView
                { title = "PRO - Reference"
                , info = """This value is reference energy for the amino acid Proline (PRO). In the EvoEF2
                            output this value is called `reference_PRO`."""
                , mouseEnterMsg = EvoEF2RefPRO
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2RefGLNHoverBox : List (Attribute Msg)
        evoEF2RefGLNHoverBox =
            hoverInfoView
                { title = "GLN - Reference"
                , info = """This value is reference energy for the amino acid Glutamine (GLN). In the EvoEF2
                            output this value is called `reference_GLN`."""
                , mouseEnterMsg = EvoEF2RefGLN
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2RefARGHoverBox : List (Attribute Msg)
        evoEF2RefARGHoverBox =
            hoverInfoView
                { title = "ARG - Reference"
                , info = """This value is reference energy for the amino acid Arginine (ARG). In the EvoEF2
                            output this value is called `reference_ARG`."""
                , mouseEnterMsg = EvoEF2RefARG
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2RefSERHoverBox : List (Attribute Msg)
        evoEF2RefSERHoverBox =
            hoverInfoView
                { title = "SER - Reference"
                , info = """This value is reference energy for the amino acid Serine  (SER). In the EvoEF2
                            output this value is called `reference_SER`."""
                , mouseEnterMsg = EvoEF2RefSER
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2RefTHRHoverBox : List (Attribute Msg)
        evoEF2RefTHRHoverBox =
            hoverInfoView
                { title = "THR - Reference"
                , info = """This value is reference energy for the amino acid Threonine (THR). In the EvoEF2
                            output this value is called `reference_THR`."""
                , mouseEnterMsg = EvoEF2RefTHR
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2RefVALHoverBox : List (Attribute Msg)
        evoEF2RefVALHoverBox =
            hoverInfoView
                { title = "VAL - Reference"
                , info = """This value is reference energy for the amino acid Valine (VAL). In the EvoEF2
                            output this value is called `reference_VAL`."""
                , mouseEnterMsg = EvoEF2RefVAL
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2RefTRPHoverBox : List (Attribute Msg)
        evoEF2RefTRPHoverBox =
            hoverInfoView
                { title = "TRP - Reference"
                , info = """This value is reference energy for the amino acid Tryptophan (TRP). In the EvoEF2
                            output this value is called `reference_TRP`."""
                , mouseEnterMsg = EvoEF2RefTRP
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2RefTYRHoverBox : List (Attribute Msg)
        evoEF2RefTYRHoverBox =
            hoverInfoView
                { title = "TYR - Reference"
                , info = """This value is reference energy for the amino acid Tyrosine (TYR). In the EvoEF2
                            output this value is called `reference_TYR`."""
                , mouseEnterMsg = EvoEF2RefTYR
                , hoverInfoOption = hoverInfoOption
                }
    in
    [ el evoEF2RefALAHoverBox <| createTableFloatColumn metrics.evoEF2Results.reference_ALA "ALA"
    , el evoEF2RefARGHoverBox <| createTableFloatColumn metrics.evoEF2Results.reference_ARG "ARG"
    , el evoEF2RefASNHoverBox <| createTableFloatColumn metrics.evoEF2Results.reference_ASN "ASN"
    , el evoEF2RefASPHoverBox <| createTableFloatColumn metrics.evoEF2Results.reference_ASP "ASP"
    , el evoEF2RefCYSHoverBox <| createTableFloatColumn metrics.evoEF2Results.reference_CYS "CYS"
    , el evoEF2RefGLNHoverBox <| createTableFloatColumn metrics.evoEF2Results.reference_GLN "GLN"
    , el evoEF2RefGLUHoverBox <| createTableFloatColumn metrics.evoEF2Results.reference_GLU "GLU"
    , el evoEF2RefGLYHoverBox <| createTableFloatColumn metrics.evoEF2Results.reference_GLY "GLY"
    , el evoEF2RefHISHoverBox <| createTableFloatColumn metrics.evoEF2Results.reference_HIS "HIS"
    , el evoEF2RefILEHoverBox <| createTableFloatColumn metrics.evoEF2Results.reference_ILE "ILE"
    , el evoEF2RefLEUHoverBox <| createTableFloatColumn metrics.evoEF2Results.reference_LEU "LEU"
    , el evoEF2RefLYSHoverBox <| createTableFloatColumn metrics.evoEF2Results.reference_LYS "LYS"
    , el evoEF2RefMETHoverBox <| createTableFloatColumn metrics.evoEF2Results.reference_MET "MET"
    , el evoEF2RefPHEHoverBox <| createTableFloatColumn metrics.evoEF2Results.reference_PHE "PHE"
    , el evoEF2RefPROHoverBox <| createTableFloatColumn metrics.evoEF2Results.reference_PRO "PRO"
    , el evoEF2RefSERHoverBox <| createTableFloatColumn metrics.evoEF2Results.reference_SER "SER"
    , el evoEF2RefTHRHoverBox <| createTableFloatColumn metrics.evoEF2Results.reference_THR "THR"
    , el evoEF2RefTRPHoverBox <| createTableFloatColumn metrics.evoEF2Results.reference_TRP "TRP"
    , el evoEF2RefTYRHoverBox <| createTableFloatColumn metrics.evoEF2Results.reference_TYR "TYR"
    , el evoEF2RefVALHoverBox <| createTableFloatColumn metrics.evoEF2Results.reference_VAL "VAL"
    ]


evoef2IntraRColumns :
    Metrics.DesignMetrics
    -> HoverInfoOption
    -> List (Element Msg)
evoef2IntraRColumns metrics hoverInfoOption =
    let
        evoEF2IntraRVDWAttHoverBox : List (Attribute Msg)
        evoEF2IntraRVDWAttHoverBox =
            hoverInfoView
                { title = "VDW Attractive - Intra Residue"
                , info = """This value is the Van der Waals attractive energy for intra residue interactions. 
                            In the EvoEF2 output this value is called `intraR_vdwatt`."""
                , mouseEnterMsg = EvoEF2IntraRVDWAtt
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2IntraRVDWRepHoverBox : List (Attribute Msg)
        evoEF2IntraRVDWRepHoverBox =
            hoverInfoView
                { title = "VDW Repulsive - Intra Residue"
                , info = """This value is the Van der Waals repulsive energy for intra residue interactions. 
                            In the EvoEF2 output this value is called `intraR_vdwrep`."""
                , mouseEnterMsg = EvoEF2IntraRVDWRep
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2IntraRElecHoverBox : List (Attribute Msg)
        evoEF2IntraRElecHoverBox =
            hoverInfoView
                { title = "Electrostatics - Intra Residue"
                , info = """This value is the Coulomb’s electrostatics energy for intra residue interactions. 
                            In the EvoEF2 output this value is called `intraR_electr`."""
                , mouseEnterMsg = EvoEF2IntraRElec
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2IntraRDesolvPHoverBox : List (Attribute Msg)
        evoEF2IntraRDesolvPHoverBox =
            hoverInfoView
                { title = "Desolvation Polar - Intra Residue"
                , info = """This value is the polar atoms desolvation energy for intra residue interactions. 
                            In the EvoEF2 output this value is called `intraR_deslvP`."""
                , mouseEnterMsg = EvoEF2IntraRDesolvP
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2IntraRDesolvHHoverBox : List (Attribute Msg)
        evoEF2IntraRDesolvHHoverBox =
            hoverInfoView
                { title = "Desolvation Non Polar - Intra Residue"
                , info = """This value is the non polar atoms desolvation energy for intra residue interactions. 
                            In the EvoEF2 output this value is called `intraR_deslvH`."""
                , mouseEnterMsg = EvoEF2IntraRDesolvH
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2IntraRAAPropHoverBox : List (Attribute Msg)
        evoEF2IntraRAAPropHoverBox =
            hoverInfoView
                { title = "Amino Acid Propensity - Intra Residue"
                , info = """This value is the amino acid propensity energy for intra residue interactions. 
                            In the EvoEF2 output this value is called `aapropensity`."""
                , mouseEnterMsg = EvoEF2AAProp
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2IntraRRamaHoverBox : List (Attribute Msg)
        evoEF2IntraRRamaHoverBox =
            hoverInfoView
                { title = "Ramachandran - Intra Residue"
                , info = """This value is the Ramachandran energy for intra residue interactions. 
                            In the EvoEF2 output this value is called `ramachandran`."""
                , mouseEnterMsg = EvoEF2Rama
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2IntraRDunbrackHoverBox : List (Attribute Msg)
        evoEF2IntraRDunbrackHoverBox =
            hoverInfoView
                { title = "Dunbrack Rotamer - Intra Residue"
                , info = """This value is the Dunbrack Rotamer energy for intra residue interactions. 
                            In the EvoEF2 output this value is called `dunbrack`."""
                , mouseEnterMsg = EvoEF2Dunbrack
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2IntraRHBSCBBDisHoverBox : List (Attribute Msg)
        evoEF2IntraRHBSCBBDisHoverBox =
            hoverInfoView
                { title = "HB Sidechain Backbone Distance - Intra Residue"
                , info = """This value is the energy for the hydrogen-acceptor distance
                            from sidechain - backbone and intra residue interactions. 
                            In the EvoEF2 output this value is called `intraR_hbscbb_dis`."""
                , mouseEnterMsg = EvoEF2IntraRHBSCBBDis
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2IntraRHBSCBBTheHoverBox : List (Attribute Msg)
        evoEF2IntraRHBSCBBTheHoverBox =
            hoverInfoView
                { title = "HB Sidechain Backbone Theta - Intra Residue"
                , info = """This value is the energy for the angle between the donor, 
                            hydrogen and acceptor atoms (theta), from sidechain - backbone 
                            and intra residue interactions. In the EvoEF2 output this value is 
                            called `intraR_hbscbb_the`."""
                , mouseEnterMsg = EvoEF2IntraRHBSCBBThe
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2IntraRHBSCBBPhiHoverBox : List (Attribute Msg)
        evoEF2IntraRHBSCBBPhiHoverBox =
            hoverInfoView
                { title = "HB Sidechain Backbone Phi - Intra Residue"
                , info = """This value is the energy for the angle between the hydrogen, 
                            acceptor and base atoms (phi), from sidechain - backbone 
                            and intra residue interactions. In the EvoEF2 output this value is 
                            called `intraR_hbscbb_phi`."""
                , mouseEnterMsg = EvoEF2IntraRHBSCBBPhi
                , hoverInfoOption = hoverInfoOption
                }
    in
    [ el evoEF2IntraRVDWAttHoverBox <| createTableFloatColumn metrics.evoEF2Results.intraR_vdwatt "VDW Attractive"
    , el evoEF2IntraRVDWRepHoverBox <| createTableFloatColumn metrics.evoEF2Results.intraR_vdwrep "VDW Repulsive"
    , el evoEF2IntraRElecHoverBox <| createTableFloatColumn metrics.evoEF2Results.intraR_electr "Electrostatics"
    , el evoEF2IntraRDesolvPHoverBox <| createTableFloatColumn metrics.evoEF2Results.intraR_deslvP "Desolvation Polar"
    , el evoEF2IntraRDesolvHHoverBox <| createTableFloatColumn metrics.evoEF2Results.intraR_deslvH "Desolvation Non Polar"
    , el evoEF2IntraRAAPropHoverBox <| createTableFloatColumn metrics.evoEF2Results.aapropensity "Amino Acid Propensity"
    , el evoEF2IntraRRamaHoverBox <| createTableFloatColumn metrics.evoEF2Results.ramachandran "Ramachandran"
    , el evoEF2IntraRDunbrackHoverBox <| createTableFloatColumn metrics.evoEF2Results.dunbrack "Dunbrack Rotamer"
    , el evoEF2IntraRHBSCBBDisHoverBox <| createTableFloatColumn metrics.evoEF2Results.intraR_hbscbb_dis "HB Sidechain Backbone Distance"
    , el evoEF2IntraRHBSCBBTheHoverBox <| createTableFloatColumn metrics.evoEF2Results.intraR_hbscbb_the "HB Sidechain Backbone Theta"
    , el evoEF2IntraRHBSCBBPhiHoverBox <| createTableFloatColumn metrics.evoEF2Results.intraR_hbscbb_phi "HB Sidechain Backbone Phi"
    ]


evoef2InterSColumns :
    Metrics.DesignMetrics
    -> HoverInfoOption
    -> List (Element Msg)
evoef2InterSColumns metrics hoverInfoOption =
    let
        evoEF2InterSVDWAttHoverBox : List (Attribute Msg)
        evoEF2InterSVDWAttHoverBox =
            hoverInfoView
                { title = "VDW Attractive - Inter Residues - Same Chain"
                , info = """This value is the Van der Waals attractive energy for inter residue interactions - same chain. 
                            In the EvoEF2 output this value is called `interS_vdwatt`."""
                , mouseEnterMsg = EvoEF2InterSVDWAtt
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2InterSVDWRepHoverBox : List (Attribute Msg)
        evoEF2InterSVDWRepHoverBox =
            hoverInfoView
                { title = "VDW Repulsive - Inter Residues - Same Chain"
                , info = """This value is the Van der Waals repulsive energy for inter residue interactions - same chain. 
                            In the EvoEF2 output this value is called `interS_vdwrep`."""
                , mouseEnterMsg = EvoEF2InterSVDWRep
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2InterSElecHoverBox : List (Attribute Msg)
        evoEF2InterSElecHoverBox =
            hoverInfoView
                { title = "Electrostatics - Inter Residues - Same Chain"
                , info = """This value is the Coulomb’s electrostatics energy for inter residue interactions - same chain. 
                            In the EvoEF2 output this value is called `interS_electr`."""
                , mouseEnterMsg = EvoEF2InterSElec
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2InterSDesolvPHoverBox : List (Attribute Msg)
        evoEF2InterSDesolvPHoverBox =
            hoverInfoView
                { title = "Desolvation Polar - Inter Residues - Same Chain"
                , info = """This value is the polar atoms desolvation energy for inter residue interactions - same chain. 
                            In the EvoEF2 output this value is called `interS_deslvP`."""
                , mouseEnterMsg = EvoEF2InterSDesolvP
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2InterSDesolvHHoverBox : List (Attribute Msg)
        evoEF2InterSDesolvHHoverBox =
            hoverInfoView
                { title = "Desolvation Non Polar - Inter Residues - Same Chain"
                , info = """This value is the non polar atoms desolvation energy for inter residue interactions - same chain. 
                            In the EvoEF2 output this value is called `interS_deslvH`."""
                , mouseEnterMsg = EvoEF2InterSDesolvH
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2InterSSSbondHHoverBox : List (Attribute Msg)
        evoEF2InterSSSbondHHoverBox =
            hoverInfoView
                { title = "Disulfide Bonding - Inter Residues - Same Chain"
                , info = """This value is the disulfide bonding energy for inter residue interactions - same chain. 
                            In the EvoEF2 output this value is called `interS_ssbond`."""
                , mouseEnterMsg = EvoEF2InterSSSbond
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2InterSHBBBBBDisHoverBox : List (Attribute Msg)
        evoEF2InterSHBBBBBDisHoverBox =
            hoverInfoView
                { title = "HB Backbone Backbone Distance - Inter Residues - Same Chain"
                , info = """This value is the energy for the hydrogen-acceptor distance
                            from backbone - backbone and inter residue interactions - same chain. 
                            In the EvoEF2 output this value is called `interS_hbbbbb_dis`."""
                , mouseEnterMsg = EvoEF2InterSHBBBBBDis
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2InterSHBBBBBTheHoverBox : List (Attribute Msg)
        evoEF2InterSHBBBBBTheHoverBox =
            hoverInfoView
                { title = "HB Backbone Backbone Theta - Inter Residues - Same Chain"
                , info = """This value is the energy for the angle between the donor, 
                            hydrogen and acceptor atoms (theta), from backbone - backbone 
                            and inter residue interactions - same chain. 
                            In the EvoEF2 output this value is called `interS_hbbbbb_the`."""
                , mouseEnterMsg = EvoEF2InterSHBBBBBThe
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2InterSHBBBBBPhiHoverBox : List (Attribute Msg)
        evoEF2InterSHBBBBBPhiHoverBox =
            hoverInfoView
                { title = "HB Backbone Backbone Phi - Inter Residues - Same Chain"
                , info = """This value is the energy for the angle between the hydrogen, 
                            acceptor and base atoms (phi), from backbone - backbone 
                            and inter residue interactions - same chain. 
                            In the EvoEF2 output this value is called `interS_hbbbbb_phi`."""
                , mouseEnterMsg = EvoEF2InterSHBBBBBPhi
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2InterSHBSCBBDisHoverBox : List (Attribute Msg)
        evoEF2InterSHBSCBBDisHoverBox =
            hoverInfoView
                { title = "HB Sidechain Backbone Distance - Inter Residues - Same Chain"
                , info = """This value is the energy for the hydrogen-acceptor distance
                            from side chain - backbone and inter residue interactions - same chain. 
                            In the EvoEF2 output this value is called `interS_hbscbb_dis`."""
                , mouseEnterMsg = EvoEF2InterSHBSCBBDis
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2InterSHBSCBBTheHoverBox : List (Attribute Msg)
        evoEF2InterSHBSCBBTheHoverBox =
            hoverInfoView
                { title = "HB Sidechain Backbone Theta - Inter Residues - Same Chain"
                , info = """This value is the energy for the angle between the donor, 
                            hydrogen and acceptor atoms (theta), from side chain - backbone 
                            and inter residue interactions - same chain. 
                            In the EvoEF2 output this value is called `interS_hbscbb_the`."""
                , mouseEnterMsg = EvoEF2InterSHBSCBBThe
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2InterSHBSCBBPhiHoverBox : List (Attribute Msg)
        evoEF2InterSHBSCBBPhiHoverBox =
            hoverInfoView
                { title = "HB Sidechain Backbone Phi - Inter Residues - Same Chain"
                , info = """This value is the energy for the angle between the hydrogen, 
                            acceptor and base atoms (phi), from side chain - backbone 
                            and inter residue interactions - same chain. 
                            In the EvoEF2 output this value is called `interS_hbscbb_phi`."""
                , mouseEnterMsg = EvoEF2InterSHBSCBBPhi
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2InterSHBSCSCDisHoverBox : List (Attribute Msg)
        evoEF2InterSHBSCSCDisHoverBox =
            hoverInfoView
                { title = "HB Sidechain Sidechain Distance - Inter Residues - Same Chain"
                , info = """This value is the energy for the hydrogen-acceptor distance
                            from side chain - side chain and inter residue interactions - same chain. 
                            In the EvoEF2 output this value is called `interS_hbscsc_dis`."""
                , mouseEnterMsg = EvoEF2InterSHBSCSCDis
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2InterSHBSCSCTheHoverBox : List (Attribute Msg)
        evoEF2InterSHBSCSCTheHoverBox =
            hoverInfoView
                { title = "HB Sidechain Sidechain Theta - Inter Residues - Same Chain"
                , info = """This value is the energy for the angle between the donor, 
                            hydrogen and acceptor atoms (theta), from side chain - side chain 
                            and inter residue interactions - same chain. 
                            In the EvoEF2 output this value is called `interS_hbscsc_the`."""
                , mouseEnterMsg = EvoEF2InterSHBSCSCThe
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2InterSHBSCSCPhiHoverBox : List (Attribute Msg)
        evoEF2InterSHBSCSCPhiHoverBox =
            hoverInfoView
                { title = "HB Sidechain Sidechain Phi - Inter Residues - Same Chain"
                , info = """This value is the energy for the angle between the hydrogen, 
                            acceptor and base atoms (phi), from side chain - side chain
                            and inter residue interactions - same chain. 
                            In the EvoEF2 output this value is called `interS_hbscsc_phi`."""
                , mouseEnterMsg = EvoEF2InterSHBSCSCPhi
                , hoverInfoOption = hoverInfoOption
                }
    in
    [ el evoEF2InterSVDWAttHoverBox <| createTableFloatColumn metrics.evoEF2Results.interS_vdwatt "VDW Attractive"
    , el evoEF2InterSVDWRepHoverBox <| createTableFloatColumn metrics.evoEF2Results.interS_vdwrep "VDW Repulsive"
    , el evoEF2InterSElecHoverBox <| createTableFloatColumn metrics.evoEF2Results.interS_electr "Electrostatics"
    , el evoEF2InterSDesolvPHoverBox <| createTableFloatColumn metrics.evoEF2Results.interS_deslvP "Desolvation Polar"
    , el evoEF2InterSDesolvHHoverBox <| createTableFloatColumn metrics.evoEF2Results.interS_deslvH "Desolvation Non Polar"
    , el evoEF2InterSSSbondHHoverBox <| createTableFloatColumn metrics.evoEF2Results.interS_ssbond "Disulfide Bonding"
    , el evoEF2InterSHBBBBBDisHoverBox <| createTableFloatColumn metrics.evoEF2Results.interS_hbbbbb_dis "HB Backbone Backbone Distance"
    , el evoEF2InterSHBBBBBTheHoverBox <| createTableFloatColumn metrics.evoEF2Results.interS_hbbbbb_the "HB Backbone Backbone Theta"
    , el evoEF2InterSHBBBBBPhiHoverBox <| createTableFloatColumn metrics.evoEF2Results.interS_hbbbbb_phi "HB Backbone Backbone Phi"
    , el evoEF2InterSHBSCBBDisHoverBox <| createTableFloatColumn metrics.evoEF2Results.interS_hbscbb_dis "HB Sidechain Backbone Distance"
    , el evoEF2InterSHBSCBBTheHoverBox <| createTableFloatColumn metrics.evoEF2Results.interS_hbscbb_the "HB Sidechain Backbone Theta"
    , el evoEF2InterSHBSCBBPhiHoverBox <| createTableFloatColumn metrics.evoEF2Results.interS_hbscbb_phi "HB Sidechain Backbone Phi"
    , el evoEF2InterSHBSCSCDisHoverBox <| createTableFloatColumn metrics.evoEF2Results.interS_hbscsc_dis "HB Sidechain Sidechain Distance"
    , el evoEF2InterSHBSCSCTheHoverBox <| createTableFloatColumn metrics.evoEF2Results.interS_hbscsc_the "HB Sidechain Sidechain Theta"
    , el evoEF2InterSHBSCSCPhiHoverBox <| createTableFloatColumn metrics.evoEF2Results.interS_hbscsc_phi "HB Sidechain Sidechain Phi"
    ]


evoef2InterDColumns :
    Metrics.DesignMetrics
    -> HoverInfoOption
    -> List (Element Msg)
evoef2InterDColumns metrics hoverInfoOption =
    let
        evoEF2InterDVDWAttHoverBox : List (Attribute Msg)
        evoEF2InterDVDWAttHoverBox =
            hoverInfoView
                { title = "VDW Attractive - Inter Residues - Different Chains"
                , info = """This value is the Van der Waals attractive energy for inter residue interactions - different chains. 
                            In the EvoEF2 output this value is called `interD_vdwatt`."""
                , mouseEnterMsg = EvoEF2InterDVDWAtt
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2InterDVDWRepHoverBox : List (Attribute Msg)
        evoEF2InterDVDWRepHoverBox =
            hoverInfoView
                { title = "VDW Repulsive - Inter Residues - Different Chains"
                , info = """This value is the Van der Waals repulsive energy for inter residue interactions - different chains. 
                            In the EvoEF2 output this value is called `interD_vdwrep`."""
                , mouseEnterMsg = EvoEF2InterDVDWRep
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2InterDElecHoverBox : List (Attribute Msg)
        evoEF2InterDElecHoverBox =
            hoverInfoView
                { title = "Electrostatics - Inter Residues - Different Chains"
                , info = """This value is the Coulomb’s electrostatics energy for inter residue interactions - different chains. 
                            In the EvoEF2 output this value is called `interD_electr`."""
                , mouseEnterMsg = EvoEF2InterDElec
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2InterDDesolvPHoverBox : List (Attribute Msg)
        evoEF2InterDDesolvPHoverBox =
            hoverInfoView
                { title = "Desolvation Polar - Inter Residues - Different Chains"
                , info = """This value is the polar atoms desolvation energy for inter residue interactions - different chains. 
                            In the EvoEF2 output this value is called `interD_deslvP`."""
                , mouseEnterMsg = EvoEF2InterDDesolvP
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2InterDDesolvHHoverBox : List (Attribute Msg)
        evoEF2InterDDesolvHHoverBox =
            hoverInfoView
                { title = "Desolvation Non Polar - Inter Residues - Different Chains"
                , info = """This value is the non polar atoms desolvation energy for inter residue interactions - different chains. 
                            In the EvoEF2 output this value is called `interD_deslvH`."""
                , mouseEnterMsg = EvoEF2InterDDesolvH
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2InterDSSbondHHoverBox : List (Attribute Msg)
        evoEF2InterDSSbondHHoverBox =
            hoverInfoView
                { title = "Disulfide Bonding - Inter Residues - Different Chains"
                , info = """This value is the disulfide bonding energy for inter residue interactions - different chains. 
                            In the EvoEF2 output this value is called `interD_ssbond`."""
                , mouseEnterMsg = EvoEF2InterDSSbond
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2InterDHBBBBBDisHoverBox : List (Attribute Msg)
        evoEF2InterDHBBBBBDisHoverBox =
            hoverInfoView
                { title = "HB Backbone Backbone Distance - Inter Residues - Different Chains"
                , info = """This value is the energy for the hydrogen-acceptor distance
                            from backbone - backbone and inter residue interactions - different chains. 
                            In the EvoEF2 output this value is called `interD_hbbbbb_dis`."""
                , mouseEnterMsg = EvoEF2InterDHBBBBBDis
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2InterDHBBBBBTheHoverBox : List (Attribute Msg)
        evoEF2InterDHBBBBBTheHoverBox =
            hoverInfoView
                { title = "HB Backbone Backbone Theta - Inter Residues - Different Chains"
                , info = """This value is the energy for the angle between the donor, 
                            hydrogen and acceptor atoms (theta), from backbone - backbone 
                            and inter residue interactions - different chains. 
                            In the EvoEF2 output this value is called `interD_hbbbbb_the`."""
                , mouseEnterMsg = EvoEF2InterDHBBBBBThe
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2InterDHBBBBBPhiHoverBox : List (Attribute Msg)
        evoEF2InterDHBBBBBPhiHoverBox =
            hoverInfoView
                { title = "HB Backbone Backbone Phi - Inter Residues - Different Chains"
                , info = """This value is the energy for the angle between the hydrogen, 
                            acceptor and base atoms (phi), from backbone - backbone 
                            and inter residue interactions - different chains. 
                            In the EvoEF2 output this value is called `interD_hbbbbb_phi`."""
                , mouseEnterMsg = EvoEF2InterDHBBBBBPhi
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2InterDHBSCBBDisHoverBox : List (Attribute Msg)
        evoEF2InterDHBSCBBDisHoverBox =
            hoverInfoView
                { title = "HB Sidechain Backbone Distance - Inter Residues - Different Chains"
                , info = """This value is the energy for the hydrogen-acceptor distance
                            from side chain - backbone and inter residue interactions - different chains. 
                            In the EvoEF2 output this value is called `interD_hbscbb_dis`."""
                , mouseEnterMsg = EvoEF2InterDHBSCBBDis
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2InterDHBSCBBTheHoverBox : List (Attribute Msg)
        evoEF2InterDHBSCBBTheHoverBox =
            hoverInfoView
                { title = "HB Sidechain Backbone Theta - Inter Residues - Different Chains"
                , info = """This value is the energy for the angle between the donor, 
                            hydrogen and acceptor atoms (theta), from side chain - backbone 
                            and inter residue interactions - different chains. 
                            In the EvoEF2 output this value is called `interD_hbscbb_the`."""
                , mouseEnterMsg = EvoEF2InterDHBSCBBThe
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2InterDHBSCBBPhiHoverBox : List (Attribute Msg)
        evoEF2InterDHBSCBBPhiHoverBox =
            hoverInfoView
                { title = "HB Sidechain Backbone Phi - Inter Residues - Different Chains"
                , info = """This value is the energy for the angle between the hydrogen, 
                            acceptor and base atoms (phi), from side chain - backbone 
                            and inter residue interactions - different chains. 
                            In the EvoEF2 output this value is called `interD_hbscbb_phi`."""
                , mouseEnterMsg = EvoEF2InterDHBSCBBPhi
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2InterDHBSCSCDisHoverBox : List (Attribute Msg)
        evoEF2InterDHBSCSCDisHoverBox =
            hoverInfoView
                { title = "HB Sidechain Sidechain Distance - Inter Residues - Different Chains"
                , info = """This value is the energy for the hydrogen-acceptor distance
                            from side chain - side chain and inter residue interactions - different chains. 
                            In the EvoEF2 output this value is called `interD_hbscsc_dis`."""
                , mouseEnterMsg = EvoEF2InterDHBSCSCDis
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2InterDHBSCSCTheHoverBox : List (Attribute Msg)
        evoEF2InterDHBSCSCTheHoverBox =
            hoverInfoView
                { title = "HB Sidechain Sidechain Theta - Inter Residues - Different Chains"
                , info = """This value is the energy for the angle between the donor, 
                            hydrogen and acceptor atoms (theta), from side chain - side chain 
                            and inter residue interactions - different chains. 
                            In the EvoEF2 output this value is called `interD_hbscsc_the`."""
                , mouseEnterMsg = EvoEF2InterDHBSCSCThe
                , hoverInfoOption = hoverInfoOption
                }

        evoEF2InterDHBSCSCPhiHoverBox : List (Attribute Msg)
        evoEF2InterDHBSCSCPhiHoverBox =
            hoverInfoView
                { title = "HB Sidechain Sidechain Phi - Inter Residues - Different Chains"
                , info = """This value is the energy for the angle between the hydrogen, 
                            acceptor and base atoms (phi), from side chain - side chain
                            and inter residue interactions - different chains. 
                            In the EvoEF2 output this value is called `interD_hbscsc_phi`."""
                , mouseEnterMsg = EvoEF2InterDHBSCSCPhi
                , hoverInfoOption = hoverInfoOption
                }
    in
    [ el evoEF2InterDVDWAttHoverBox <| createTableFloatColumn metrics.evoEF2Results.interD_vdwatt "VDW Attractive"
    , el evoEF2InterDVDWRepHoverBox <| createTableFloatColumn metrics.evoEF2Results.interD_vdwrep "VDW Repulsive"
    , el evoEF2InterDElecHoverBox <| createTableFloatColumn metrics.evoEF2Results.interD_electr "Electrostatics"
    , el evoEF2InterDDesolvPHoverBox <| createTableFloatColumn metrics.evoEF2Results.interD_deslvP "Desolvation Polar"
    , el evoEF2InterDDesolvHHoverBox <| createTableFloatColumn metrics.evoEF2Results.interD_deslvH "Desolvation Non Polar"
    , el evoEF2InterDSSbondHHoverBox <| createTableFloatColumn metrics.evoEF2Results.interD_ssbond "Disulfide Bonding"
    , el evoEF2InterDHBBBBBDisHoverBox <| createTableFloatColumn metrics.evoEF2Results.interD_hbbbbb_dis "HB Backbone Backbone Distance"
    , el evoEF2InterDHBBBBBTheHoverBox <| createTableFloatColumn metrics.evoEF2Results.interD_hbbbbb_the "HB Backbone Backbone Theta"
    , el evoEF2InterDHBBBBBPhiHoverBox <| createTableFloatColumn metrics.evoEF2Results.interD_hbbbbb_phi "HB Backbone Backbone Phi"
    , el evoEF2InterDHBSCBBDisHoverBox <| createTableFloatColumn metrics.evoEF2Results.interD_hbscbb_dis "HB Sidechain Backbone Distance"
    , el evoEF2InterDHBSCBBTheHoverBox <| createTableFloatColumn metrics.evoEF2Results.interD_hbscbb_the "HB Sidechain Backbone Theta"
    , el evoEF2InterDHBSCBBPhiHoverBox <| createTableFloatColumn metrics.evoEF2Results.interD_hbscbb_phi "HB Sidechain Backbone Phi"
    , el evoEF2InterDHBSCSCDisHoverBox <| createTableFloatColumn metrics.evoEF2Results.interD_hbscsc_dis "HB Sidechain Sidechain Distance"
    , el evoEF2InterDHBSCSCTheHoverBox <| createTableFloatColumn metrics.evoEF2Results.interD_hbscsc_the "HB Sidechain Sidechain Theta"
    , el evoEF2InterDHBSCSCPhiHoverBox <| createTableFloatColumn metrics.evoEF2Results.interD_hbscsc_phi "HB Sidechain Sidechain Phi"
    ]


dfire2LogInfoSelection : Metrics.DesignMetrics -> String
dfire2LogInfoSelection metrics =
    case ( metrics.dfire2Results.return_code, metrics.dfire2Results.error_info ) of
        ( 0, "" ) ->
            metrics.dfire2Results.log_info

        _ ->
            metrics.dfire2Results.error_info


dfire2ResultsView : Metrics.DesignMetrics -> DisplaySettings -> HoverInfoOption -> Element Msg
dfire2ResultsView metrics displaySettings hoverInfoOption =
    let
        logInfoBox =
            paragraph
                [ spacing 20
                , padding 20
                , Font.family
                    [ Font.typeface "Roboto Mono"
                    , Font.monospace
                    ]
                , Font.size 10
                ]
                [ text (dfire2LogInfoSelection metrics)
                ]

        dfire2TotalHoverBox : List (Attribute Msg)
        dfire2TotalHoverBox =
            hoverInfoView
                { title = "Total DFIRE2"
                , info = """This value is the total DFIRE2 energy. This is the only field that is returned from
                            running DFIRE2 on a pdb file."""
                , mouseEnterMsg = DFIRE2Total
                , hoverInfoOption = hoverInfoOption
                }
    in
    sectionColumn
        [ Style.h3 <|
            paragraph []
                [ text
                    "DFIRE2 Energy Function Results"
                ]
        , wrappedRow
            [ spacing 5
            , centerX
            ]
            [ el dfire2TotalHoverBox <| createTableFloatColumn metrics.dfire2Results.total "Total DFIRE2" ]
        , Folds.sectionFoldView
            { foldVisible = displaySettings.dfire2LogInfo
            , title = hideableSectionToString DFIRE2LogInfo
            , toggleMsg = ToggleSectionVisibility DFIRE2LogInfo
            , contentView = logInfoBox
            }
        ]


rosettaResultsTableView :
    Metrics.DesignMetrics
    -> DisplaySettings
    -> HoverInfoOption
    -> Element Msg
rosettaResultsTableView metrics displaySettings hoverInfoOption =
    let
        logInfoBox =
            paragraph
                [ spacing 20
                , padding 20
                , width fill
                , Font.family
                    [ Font.typeface "Roboto Mono"
                    , Font.monospace
                    ]
                , Font.size 10
                ]
                [ text metrics.rosettaResults.log_info
                ]
    in
    sectionColumn
        [ Style.h3 <|
            paragraph []
                [ text "Rosetta Energy Function Results" ]
        , wrappedRow
            [ spacing 5 ]
            (rosettaColumns metrics hoverInfoOption)
        , Folds.sectionFoldView
            { foldVisible = displaySettings.rosettaLogInfo
            , title = hideableSectionToString RosettaLogInfo
            , toggleMsg = ToggleSectionVisibility RosettaLogInfo
            , contentView = logInfoBox
            }
        ]


rosettaColumns :
    Metrics.DesignMetrics
    -> HoverInfoOption
    -> List (Element Msg)
rosettaColumns metrics hoverInfoOption =
    let
        rosettaTotalHoverBox : List (Attribute Msg)
        rosettaTotalHoverBox =
            hoverInfoView
                { title = "Total Rosetta"
                , info = """This value is the total Rosetta energy. It is a weighted sum of the different 
                            Rosetta energy values. In the Rosetta `score.sc` output file, this value is called 
                            `total_score`."""
                , mouseEnterMsg = RosettaTotal
                , hoverInfoOption = hoverInfoOption
                }

        rosettaReferenceHoverBox : List (Attribute Msg)
        rosettaReferenceHoverBox =
            hoverInfoView
                { title = "Reference"
                , info = """This value is the reference energy for the different amino acids. 
                            In the Rosetta `score.sc` output file, this value is called `ref`."""
                , mouseEnterMsg = RosettaReference
                , hoverInfoOption = hoverInfoOption
                }

        rosettaVDWAttHoverBox : List (Attribute Msg)
        rosettaVDWAttHoverBox =
            hoverInfoView
                { title = "VDW Attractive"
                , info = """This value is the attractive energy between two atoms on different residues 
                            separated by distance, d. In the Rosetta `score.sc` output file, this value 
                            is called `fa_atr`."""
                , mouseEnterMsg = RosettaVDWAtt
                , hoverInfoOption = hoverInfoOption
                }

        rosettaVDWRepHoverBox : List (Attribute Msg)
        rosettaVDWRepHoverBox =
            hoverInfoView
                { title = "VDW Repulsive"
                , info = """This value is the repulsive energy between two atoms on different residues 
                            separated by distance, d. In the Rosetta `score.sc` output file, this value 
                            is called `fa_rep`."""
                , mouseEnterMsg = RosettaVDWRep
                , hoverInfoOption = hoverInfoOption
                }

        rosettaVDWRepIntraRHoverBox : List (Attribute Msg)
        rosettaVDWRepIntraRHoverBox =
            hoverInfoView
                { title = "VDW Repulsive Intra Residue"
                , info = """This value is the repulsive energy between two atoms on the same residue 
                            separated by distance, d. In the Rosetta `score.sc` output file, this value 
                            is called `fa_intra_rep`."""
                , mouseEnterMsg = RosettaVDWRepIntraR
                , hoverInfoOption = hoverInfoOption
                }

        rosettaElecHoverBox : List (Attribute Msg)
        rosettaElecHoverBox =
            hoverInfoView
                { title = "Electrostatics"
                , info = """This value is the energy of interaction between two non-bonded charged atoms 
                            separated by distance, d. In the Rosetta `score.sc` output file, this value 
                            is called `fa_elec`."""
                , mouseEnterMsg = RosettaElec
                , hoverInfoOption = hoverInfoOption
                }

        rosettaSolvIsoHoverBox : List (Attribute Msg)
        rosettaSolvIsoHoverBox =
            hoverInfoView
                { title = "Solvation Isotropic"
                , info = """This value is the Gaussian exclusion implicit solvation energy between 
                            protein atoms in different residues. In the Rosetta `score.sc` output file, 
                            this value is called `fa_sol`."""
                , mouseEnterMsg = RosettaSolvIso
                , hoverInfoOption = hoverInfoOption
                }

        rosettaSolvAnisoHoverBox : List (Attribute Msg)
        rosettaSolvAnisoHoverBox =
            hoverInfoView
                { title = "Solvation Anisotropic Polar Atoms"
                , info = """This value is the orientation-dependent solvation of polar atoms 
                            assuming ideal water geometry. In the Rosetta `score.sc` output file, 
                            this value is called `lk_ball_wtd`."""
                , mouseEnterMsg = RosettaSolvAniso
                , hoverInfoOption = hoverInfoOption
                }

        rosettaSolvIsoIntraRHoverBox : List (Attribute Msg)
        rosettaSolvIsoIntraRHoverBox =
            hoverInfoView
                { title = "Solvation Isotropic Intra Residue"
                , info = """This value is the Gaussian exclusion implicit solvation energy between 
                            protein atoms in the same residue. In the Rosetta `score.sc` output file, 
                            this value is called `fa_sol_intraR`."""
                , mouseEnterMsg = RosettaSolvIsoIntraR
                , hoverInfoOption = hoverInfoOption
                }

        rosettaHBLRBBHoverBox : List (Attribute Msg)
        rosettaHBLRBBHoverBox =
            hoverInfoView
                { title = "HB Long Range Backbone"
                , info = """This value is the energy of long range hydrogen bonds. In the Rosetta `score.sc` 
                            output file, this value is called `hbond_lr_bb`."""
                , mouseEnterMsg = RosettaHBLRBB
                , hoverInfoOption = hoverInfoOption
                }

        rosettaHBSRBBHoverBox : List (Attribute Msg)
        rosettaHBSRBBHoverBox =
            hoverInfoView
                { title = "HB Short Range Backbone"
                , info = """This value is the energy of short range hydrogen bonds. In the Rosetta `score.sc` 
                            output file, this value is called `hbond_sr_bb`."""
                , mouseEnterMsg = RosettaHBSRBB
                , hoverInfoOption = hoverInfoOption
                }

        rosettaHBBBSCHoverBox : List (Attribute Msg)
        rosettaHBBBSCHoverBox =
            hoverInfoView
                { title = "HB Backbone Sidechain"
                , info = """This value is the energy of backbone-side chain hydrogen bonds. In the Rosetta `score.sc` 
                            output file, this value is called `hbond_bb_sc`."""
                , mouseEnterMsg = RosettaHBBBSC
                , hoverInfoOption = hoverInfoOption
                }

        rosettaHBSCSCHoverBox : List (Attribute Msg)
        rosettaHBSCSCHoverBox =
            hoverInfoView
                { title = "HB Sidechain Sidechain"
                , info = """This value is the energy of side chain-side chain hydrogen bonds. In the Rosetta `score.sc` 
                            output file, this value is called `hbond_sc`."""
                , mouseEnterMsg = RosettaHBSCSC
                , hoverInfoOption = hoverInfoOption
                }

        rosettaSSbondHoverBox : List (Attribute Msg)
        rosettaSSbondHoverBox =
            hoverInfoView
                { title = "Disulfide Bridges"
                , info = """This value is the energy of disulfide bridges. In the Rosetta `score.sc` 
                            output file, this value is called `dslf_fa13`."""
                , mouseEnterMsg = RosettaSSbond
                , hoverInfoOption = hoverInfoOption
                }

        rosettaRamaHoverBox : List (Attribute Msg)
        rosettaRamaHoverBox =
            hoverInfoView
                { title = "Backbone Torsion Preference"
                , info = """This value is the probability of backbone ϕ, ψ angles given the amino acid type. 
                            In the Rosetta `score.sc` output file, this value is called `rama_prepro`."""
                , mouseEnterMsg = RosettaRama
                , hoverInfoOption = hoverInfoOption
                }

        rosettaAAPropHoverBox : List (Attribute Msg)
        rosettaAAPropHoverBox =
            hoverInfoView
                { title = "Amino Acid Propensity"
                , info = """This value is the probability of amino acid identity given the backbone ϕ, ψ angles. 
                            In the Rosetta `score.sc` output file, this value is called `p_aa_pp`."""
                , mouseEnterMsg = RosettaAAProp
                , hoverInfoOption = hoverInfoOption
                }

        rosettaDunbrackHoverBox : List (Attribute Msg)
        rosettaDunbrackHoverBox =
            hoverInfoView
                { title = "Dunbrack Rotamer"
                , info = """This value is the probability that a chosen rotamer is native-like given 
                            backbone ϕ, ψ angles. In the Rosetta `score.sc` output file, this value 
                            is called `fa_dun`."""
                , mouseEnterMsg = RosettaDunbrack
                , hoverInfoOption = hoverInfoOption
                }

        rosettaOmegaPenHoverBox : List (Attribute Msg)
        rosettaOmegaPenHoverBox =
            hoverInfoView
                { title = "Omega Penalty"
                , info = """This value is a backbone-dependent penalty for cis ω dihedrals that deviate 
                            from 0° and trans ω dihedrals that deviate from 180°. In the Rosetta `score.sc` 
                            output file, this value is called `omega`."""
                , mouseEnterMsg = RosettaOmegaPen
                , hoverInfoOption = hoverInfoOption
                }

        rosettaOpenProPenHoverBox : List (Attribute Msg)
        rosettaOpenProPenHoverBox =
            hoverInfoView
                { title = "Open Proline Penalty"
                , info = """This value is a penalty for an open proline ring and proline ω bonding energy. 
                            In the Rosetta `score.sc` output file, this value is called `pro_close`."""
                , mouseEnterMsg = RosettaOpenProPen
                , hoverInfoOption = hoverInfoOption
                }

        rosettaTyroPenHoverBox : List (Attribute Msg)
        rosettaTyroPenHoverBox =
            hoverInfoView
                { title = "Tyrosine χ3 Dihedral Angle Penalty"
                , info = """This value is a sinusoidal penalty for non-planar tyrosine χ3 dihedral angle. 
                            In the Rosetta `score.sc` output file, this value is called `yhh_planarity`."""
                , mouseEnterMsg = RosettaTyroPen
                , hoverInfoOption = hoverInfoOption
                }
    in
    [ el rosettaTotalHoverBox <| createTableFloatColumn metrics.rosettaResults.total_score "Total Rosetta"
    , el rosettaReferenceHoverBox <| createTableFloatColumn metrics.rosettaResults.ref "Reference"
    , el rosettaVDWAttHoverBox <| createTableFloatColumn metrics.rosettaResults.fa_atr "VDW Attractive"
    , el rosettaVDWRepHoverBox <| createTableFloatColumn metrics.rosettaResults.fa_rep "VDW Repulsive"
    , el rosettaVDWRepIntraRHoverBox <| createTableFloatColumn metrics.rosettaResults.fa_intra_rep "VDW Repulsive Intra Residue"
    , el rosettaElecHoverBox <| createTableFloatColumn metrics.rosettaResults.fa_elec "Electrostatics"
    , el rosettaSolvIsoHoverBox <| createTableFloatColumn metrics.rosettaResults.fa_sol "Solvation Isotropic"
    , el rosettaSolvAnisoHoverBox <| createTableFloatColumn metrics.rosettaResults.lk_ball_wtd "Solvation Anisotropic Polar Atoms"
    , el rosettaSolvIsoIntraRHoverBox <| createTableFloatColumn metrics.rosettaResults.fa_intra_sol_xover4 "Solvation Isotropic Intra Residue"
    , el rosettaHBLRBBHoverBox <| createTableFloatColumn metrics.rosettaResults.hbond_lr_bb "HB Long Range Backbone"
    , el rosettaHBSRBBHoverBox <| createTableFloatColumn metrics.rosettaResults.hbond_sr_bb "HB Short Range Backbone"
    , el rosettaHBBBSCHoverBox <| createTableFloatColumn metrics.rosettaResults.hbond_bb_sc "HB Backbone Sidechain"
    , el rosettaHBSCSCHoverBox <| createTableFloatColumn metrics.rosettaResults.hbond_sc "HB Sidechain Sidechain"
    , el rosettaSSbondHoverBox <| createTableFloatColumn metrics.rosettaResults.dslf_fa13 "Disulfide Bridges"
    , el rosettaRamaHoverBox <| createTableFloatColumn metrics.rosettaResults.rama_prepro "Backbone Torsion Preference"
    , el rosettaAAPropHoverBox <| createTableFloatColumn metrics.rosettaResults.p_aa_pp "Amino Acid Propensity"
    , el rosettaDunbrackHoverBox <| createTableFloatColumn metrics.rosettaResults.fa_dun "Dunbrack Rotamer"
    , el rosettaOmegaPenHoverBox <| createTableFloatColumn metrics.rosettaResults.omega "Omega Penalty"
    , el rosettaOpenProPenHoverBox <| createTableFloatColumn metrics.rosettaResults.pro_close "Open Proline Penalty"
    , el rosettaTyroPenHoverBox <| createTableFloatColumn metrics.rosettaResults.yhh_planarity "Tyrosine χ3 Dihedral Angle Penalty"
    ]


referenceSetComparisonView : Element msg
referenceSetComparisonView =
    sectionColumn
        [ Style.h2 <| text "Comparison to Reference Set"
        , compositionView
        , torsionAnglesView
        , metricsHistogramsView
        ]


compositionView : Element msg
compositionView =
    column
        [ width fill ]
        [ Style.h3 <| text "Composition"
        , Keyed.el [ centerX ]
            ( "composition"
            , Html.div
                [ HAtt.id "composition"
                , HAtt.style "width" "100%"
                ]
                [ Html.div
                    [ HAtt.height 200
                    , HAtt.style "height" "200px"
                    , HAtt.style "width" "100%"
                    , HAtt.style "border-radius" "5px"
                    , HAtt.style "background-color" "#d3d3d3"
                    ]
                    []
                ]
                |> html
            )
        ]


torsionAnglesView : Element msg
torsionAnglesView =
    column
        [ width fill ]
        [ Style.h3 <| text "Backbone Torsion Angles"
        , Keyed.el [ centerX ]
            ( "torsionAngles"
            , Html.div
                [ HAtt.id "torsionAngles"
                , HAtt.style "width" "100%"
                ]
                [ Html.div
                    [ HAtt.height 200
                    , HAtt.style "height" "200px"
                    , HAtt.style "width" "100%"
                    , HAtt.style "border-radius" "5px"
                    , HAtt.style "background-color" "#d3d3d3"
                    ]
                    []
                ]
                |> html
            )
        ]


metricsHistogramsView : Element msg
metricsHistogramsView =
    column [ width fill ]
        [ Style.h3 <| text "Metrics Histograms"
        , Keyed.el [ centerX ]
            ( "metricsHistograms"
            , Html.div
                [ HAtt.id "metricsHistograms"
                ]
                [ Html.div
                    [ HAtt.height 200
                    , HAtt.style "height" "200px"
                    , HAtt.style "width" "100%"
                    , HAtt.style "border-radius" "5px"
                    , HAtt.style "background-color" "#d3d3d3"
                    ]
                    []
                ]
                |> html
            )
        ]



-- {{{ Table View Functions


createTableColumn :
    (metric -> Element msg)
    -> metric
    -> String
    -> Element msg
createTableColumn metricView metric metricName =
    let
        tableColumn =
            column
                [ alignTop
                , Font.size 18
                ]
    in
    tableColumn
        [ el
            [ padding 5
            , height <| px 80
            , width <| px 150
            , Background.color Style.colorPalette.c1
            , Font.center
            , Font.color Style.colorPalette.white
            ]
          <|
            paragraph [ centerY ] [ text metricName ]
        , el
            [ width fill
            , Border.solid
            , Border.widthEach { top = 0, bottom = 1, left = 1, right = 1 }
            ]
          <|
            metricView metric
        ]


cell : Element msg -> Element msg
cell =
    el [ centerX, padding 10 ]


intText : Int -> Element msg
intText =
    String.fromInt >> text >> (\a -> cell a)


onePlaceFloatText : Float -> Element msg
onePlaceFloatText =
    Round.round 0
        >> text
        >> (\a -> cell a)


createTableFloatColumn : Maybe Float -> String -> Element msg
createTableFloatColumn =
    createTableColumn
        (\a ->
            case a of
                Just b ->
                    onePlaceFloatText b

                Nothing ->
                    text "--"
        )



-- }}}
-- {{{ hoverInfoView


hoverInfoView :
    { title : String
    , info : String
    , mouseEnterMsg : HoverInfoOption
    , hoverInfoOption : HoverInfoOption
    }
    -> List (Attribute Msg)
hoverInfoView { title, info, mouseEnterMsg, hoverInfoOption } =
    let
        content : HoverInfoOption -> List (Attribute msg)
        content option =
            if option == mouseEnterMsg then
                [ above
                    (column
                        [ spacing 15
                        , padding 15
                        , centerX
                        , width (px 180)
                        , Border.innerShadow
                            { offset = ( 0, 0 )
                            , size = 1
                            , blur = 0
                            , color = rgba 0.5 0.5 0.5 0.4
                            }
                        , Border.color Style.colorPalette.black
                        , Border.width 1
                        , Background.color Style.colorPalette.white
                        , Font.color Style.colorPalette.black
                        ]
                        [ paragraph
                            [ Font.size 14
                            ]
                            [ text title
                            ]
                        , paragraph
                            [ Font.size 12 ]
                            [ text info ]
                        ]
                    )
                ]

            else
                []
    in
    [ Events.onMouseEnter (ChangeHoverInfo mouseEnterMsg)
    , Events.onMouseLeave (ChangeHoverInfo NoHoverInfo)
    ]
        ++ content hoverInfoOption



-- }}}
-- {{{ specificationView


specificationView : Metrics.DesignMetrics -> Specification -> Element msg
specificationView metrics { name, description, requirements } =
    sectionColumn
        [ Style.h2 <| text "Specification Evaluation"
        , column
            [ padding 15
            , spacing 10
            , width fill
            , Background.color Style.colorPalette.c5
            , Border.rounded 10
            ]
            [ Style.h3 <| text "Name"
            , paragraph [] [ text name ]
            , Style.h3 <| text "Description"
            , paragraph [] [ text description ]
            , Style.h3 <| text "Requirements"
            , requirementView metrics requirements
            ]
        ]


requirementView :
    Metrics.DesignMetrics
    -> Requirement RequirementData
    -> Element msg
requirementView metrics requirement =
    let
        requirementResolves =
            Requirement.resolveRequirement Nothing metrics requirement

        arrowRow r =
            row
                [ padding 5, spacing 15, width fill ]
                [ el []
                    (FeatherIcons.chevronRight
                        |> FeatherIcons.toHtml []
                        |> html
                    )
                , requirementView metrics r
                ]
    in
    el
        ([ width fill, Border.rounded 10 ]
            ++ (if requirementResolves then
                    [ Background.color Style.colorPalette.c3 ]

                else
                    [ Background.color Style.colorPalette.red ]
               )
        )
    <|
        case requirement of
            Requirement.Data data ->
                case data of
                    Requirement.Constant constantType ->
                        let
                            typeString =
                                "Constant:"

                            requirementString =
                                case constantType of
                                    Requirement.Method methodType ->
                                        let
                                            constantTypeString =
                                                typeString ++ "Method:"
                                        in
                                        case methodType of
                                            Requirement.SPPS ->
                                                constantTypeString ++ "SPPS"

                                            Requirement.MolecularBiology ->
                                                constantTypeString ++ "MolBio"
                        in
                        el [ padding 10 ] (text <| requirementString)

                    Requirement.Value valueType ->
                        let
                            typeString =
                                "Value:"

                            requirementString =
                                case valueType of
                                    Requirement.IsoelectricPoint order value ->
                                        typeString
                                            ++ "IsoelectricPoint:"
                                            ++ Requirement.stringFromOrder
                                                order
                                            ++ ":"
                                            ++ String.fromFloat value

                                    Requirement.HydrophobicFitness order value ->
                                        typeString
                                            ++ "HydrophobicFitness:"
                                            ++ Requirement.stringFromOrder
                                                order
                                            ++ ":"
                                            ++ String.fromFloat value

                                    Requirement.MeanPackingDensity order value ->
                                        typeString
                                            ++ "MeanPackingDensity:"
                                            ++ Requirement.stringFromOrder
                                                order
                                            ++ ":"
                                            ++ String.fromFloat value

                                    Requirement.SequenceContains string ->
                                        typeString
                                            ++ "SequenceContains:"
                                            ++ string

                                    Requirement.CompositionDeviation unitType value ->
                                        typeString
                                            ++ "CompositionDeviation:"
                                            ++ Requirement.stringFromUnitType unitType
                                            ++ ":"
                                            ++ String.fromFloat value
                        in
                        el (Style.defaultBorder ++ [ padding 10, width fill ])
                            (text <| requirementString)

            Requirement.Not subRequirement ->
                row (Style.defaultBorder ++ [ padding 10, spacing 10, width fill ])
                    [ Style.h3 <| el [ Font.bold ] (text <| "NOT")
                    , requirementView metrics subRequirement
                    ]

            Requirement.Or subRequirement1 subRequirement2 ->
                column
                    (Style.defaultBorder
                        ++ [ padding 10
                           , spacing 10
                           , width fill
                           ]
                    )
                    [ requirementView metrics subRequirement1
                    , Style.h3 <| el [ Font.bold ] (text "---- OR ----")
                    , requirementView metrics subRequirement2
                    ]

            Requirement.And requirement1 requirement2 ->
                column
                    (Style.defaultBorder
                        ++ [ padding 10
                           , spacing 10
                           , width fill
                           ]
                    )
                    [ requirementView metrics requirement1
                    , el [ Font.bold ]
                        (text "---- AND ----")
                    , requirementView metrics requirement2
                    ]

            Requirement.Any requirements ->
                column
                    (Style.defaultBorder
                        ++ [ padding 10
                           , spacing 10
                           , width fill
                           ]
                    )
                    [ el [ Font.bold ] (text "ANY")
                    , column [ padding 10, spacing 10, width fill ] <|
                        List.map arrowRow requirements
                    ]

            Requirement.All requirements ->
                column
                    (Style.defaultBorder
                        ++ [ padding 10
                           , spacing 10
                           , width fill
                           ]
                    )
                    [ el [ Font.bold ] (text "ALL")
                    , column [ padding 10, spacing 5, width fill ] <|
                        List.map arrowRow requirements
                    ]



-- }}}
-- }}}
