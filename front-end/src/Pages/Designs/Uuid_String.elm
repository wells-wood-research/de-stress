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
import Shared.Editable as Editable exposing (Editable(..))
import Shared.Error as Error
import Shared.Folds as Folds
import Shared.Metrics as Metrics
import Shared.Plots as Plots
import Shared.ReferenceSet as ReferenceSet exposing (ReferenceSet)
import Shared.Requirement as Requirement exposing (Requirement, RequirementData)
import Shared.Specification as Specification exposing (Specification)
import Shared.Stored as Stored exposing (Stored(..))
import Shared.Style as Style
import Shared.Tooltips as Tooltips
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
    , hoverInfoOption : Tooltips.HoverInfoOption
    , pageErrors : List Error.Error
    , device : Device
    }


type PageState
    = AppNotRunning
    | LoadingNoStub
    | LoadingWithStub Design.DesignStub
    | UnknownDesignUuid
    | Design Design.Design


mapIfDesign : (Design.Design -> Design.Design) -> PageState -> PageState
mapIfDesign fn pageState =
    case pageState of
        Design design ->
            Design <| fn design

        _ ->
            pageState


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
    | Aggrescan3dLogInfo


type alias DisplaySettings =
    { evoEF2LogInfo : Bool
    , dfire2LogInfo : Bool
    , rosettaLogInfo : Bool
    , aggrescan3dLogInfo : Bool
    }


hideableSectionToString : HideableSection -> String
hideableSectionToString hideableSection =
    case hideableSection of
        EvoEF2LogInfo ->
            "EvoEF2 Log Information"

        DFIRE2LogInfo ->
            "DFIRE2 Log Information"

        RosettaLogInfo ->
            "Rosetta Log Information"

        Aggrescan3dLogInfo ->
            "Aggrescan3D 2.0 Log Information"


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
                        , aggrescan3dLogInfo = False
                        }
                    , hoverInfoOption =
                        Tooltips.NoHoverInfo
                    , pageErrors = []
                    , device = classifyDevice shared
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
                    , aggrescan3dLogInfo = False
                    }
              , hoverInfoOption =
                    Tooltips.NoHoverInfo
              , pageErrors = []
              , device = classifyDevice shared
              }
            , Cmd.none
            )



-- }}}
-- {{{ UPDATE


type Msg
    = SetFocus { uuidString : String, design : Value }
    | SetSpecification { uuidString : String, specValue : Value }
    | SetReferenceSet { uuidString : String, refSetValue : Value }
    | ClickedNameEdit
    | EditedName String
    | ClickedCancelNameEdit
    | ClickedAcceptNameEdit
    | SetEvoEF2TableOption EvoEF2TableOption
    | ToggleSectionVisibility HideableSection
    | ChangeHoverInfo Tooltips.HoverInfoOption
    | ClearPageErrors


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
                                plotCommands model.device metrics referenceSet

                            _ ->
                                Cmd.none
                        ]
                    )

                Err _ ->
                    ( { model | pageState = UnknownDesignUuid }
                    , Cmd.none
                    )

        SetSpecification { specValue } ->
            case model.mSelectedSpecification of
                Just selectedSpec ->
                    case Codec.decodeValue Specification.codec specValue of
                        Ok specification ->
                            ( { model
                                | mSelectedSpecification =
                                    InMemory
                                        { uuidString =
                                            Stored.getUuid selectedSpec
                                        , data = specification
                                        }
                                        |> Just
                              }
                            , Cmd.none
                            )

                        Err _ ->
                            ( { model
                                | mSelectedSpecification =
                                    FailedToLoad
                                        { uuidString =
                                            Stored.getUuid selectedSpec
                                        }
                                        |> Just
                              }
                            , Cmd.none
                            )

                Nothing ->
                    Error.updateWithError
                        ClearPageErrors
                        model
                        { title = "Error setting specification"
                        , details =
                            """A specification was set, but I was not expecting
                                one."""
                        , severity = Error.Low
                        }

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
                                    plotCommands model.device metrics referenceSet

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
                    Error.updateWithError
                        ClearPageErrors
                        model
                        { title = "Error setting reference set"
                        , details =
                            """A reference set was set, but I was not expecting one."""
                        , severity = Error.Low
                        }

        ClickedNameEdit ->
            ( { model
                | pageState =
                    mapIfDesign
                        (\d -> { d | name = Editable.startEditing d.name })
                        model.pageState
              }
            , Cmd.none
            )

        EditedName updatedName ->
            ( { model
                | pageState =
                    mapIfDesign
                        (\d ->
                            { d
                                | name =
                                    Editable.editValue
                                        (if String.isEmpty updatedName then
                                            Nothing

                                         else
                                            Just updatedName
                                        )
                                        d.name
                            }
                        )
                        model.pageState
              }
            , Cmd.none
            )

        ClickedCancelNameEdit ->
            ( { model
                | pageState =
                    mapIfDesign
                        (\d -> { d | name = Editable.cancelEdit d.name })
                        model.pageState
              }
            , Cmd.none
            )

        ClickedAcceptNameEdit ->
            case model.pageState of
                Design design ->
                    let
                        updatedDesign =
                            { design
                                | name =
                                    Editable.acceptEdit
                                        design.name
                            }
                    in
                    ( { model
                        | pageState =
                            Design updatedDesign
                      }
                    , Design.updateDesignName
                        { uuidString = model.designUuid
                        , name =
                            Editable.getValue updatedDesign.name
                        }
                    )

                _ ->
                    Error.updateWithError
                        ClearPageErrors
                        model
                        { title = "Failed to rename design"
                        , details =
                            """I failed to rename this design as the page was not ready
                            and things happened out of the expected order. Try
                            refreshing your browser to fix this, or if it persists,
                            report it as a bug. See the home page for details on how to
                            do this.
                            """
                        , severity = Error.Low
                        }

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

                        Aggrescan3dLogInfo ->
                            { displaySettings
                                | aggrescan3dLogInfo =
                                    not displaySettings.aggrescan3dLogInfo
                            }
              }
            , Cmd.none
            )

        ChangeHoverInfo option ->
            ( { model | hoverInfoOption = option }
            , Cmd.none
            )

        ClearPageErrors ->
            ( { model | pageErrors = [] }, Cmd.none )


plotCommands : Device -> Metrics.DesignMetrics -> ReferenceSet -> Cmd msg
plotCommands device metrics referenceSet =
    Cmd.batch
        [ Plots.vegaPlot <|
            { plotId = "composition"
            , spec =
                Metrics.createCompositionSpec
                    device
                    referenceSet.aggregateData
                    (Just metrics)
            }
        , Plots.vegaPlot <|
            { plotId = "torsionAngles"
            , spec =
                Metrics.createTorsionAngleSpec
                    device
                    (Just metrics)
                    referenceSet.metrics
            }
        , Plots.vegaPlot <|
            { plotId = "metricsHistograms"
            , spec =
                Metrics.createAllHistogramsSpec
                    device
                    [ Metrics.makeHistPlotData
                        { hydrophobicFitness = metrics.hydrophobicFitness
                        , isoelectricPoint = metrics.isoelectricPoint
                        , mass = metrics.mass
                        , numberOfResidues = metrics.numOfResidues
                        , packingDensity = metrics.packingDensity
                        , budeFFTotalEnergy = metrics.budeFFResults.totalEnergy
                        , evoEFTotalEnergy = metrics.evoEF2Results.total
                        , dfireTotalEnergy = metrics.dfire2Results.total
                        , rosettaTotalEnergy = metrics.rosettaResults.total_score
                        , aggrescan3dTotalValue = metrics.aggrescan3dResults.total_value
                        }
                    ]
                    (referenceSet.metrics
                        |> List.map Metrics.makeHistPlotData
                    )
            }
        ]


save : Model -> Shared.Model -> Shared.Model
save model shared =
    let
        updatedShared =
            Error.updateSharedModelErrors model shared
    in
    case model.pageState of
        Design design ->
            Shared.mapRunState
                (\runState ->
                    { runState
                        | designs =
                            Dict.insert
                                model.designUuid
                                (design
                                    |> Design.createDesignStub
                                    |> Design.storeDesignStubLocally
                                )
                                runState.designs
                    }
                )
                updatedShared

        _ ->
            updatedShared


load : Shared.Model -> Model -> ( Model, Cmd Msg )
load shared model =
    ( { model
        | device = classifyDevice shared
      }
    , case model.pageState of
        Design design ->
            case
                ( model.mSelectedReferenceSet
                    |> Maybe.andThen Stored.getData
                , WebSockets.getDesignMetrics design.metricsJobStatus
                )
            of
                ( Just referenceSet, Just metrics ) ->
                    plotCommands model.device metrics referenceSet

                _ ->
                    Cmd.none

        _ ->
            Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
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
        [ wrappedRow [ spacing 10 ]
            [ Style.h1 <| text "Design Details"
            , Buttons.linkButton
                { label = text "Back"
                , route = Route.Designs
                }
            ]
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
    -> Tooltips.HoverInfoOption
    -> Element Msg
designDetailsView _ mSpecification mReferenceSet design evoEF2TableOption displaySettings hoverInfoOption =
    let
        { fileName, metricsJobStatus } =
            design
    in
    column
        [ spacing 15, width fill ]
    <|
        [ sectionColumn
            [ row [ height fill, spacing 10 ]
                (case design.name of
                    NotEditing currentName ->
                        [ paragraph [] [ Style.h2 <| text <| "Name: " ++ currentName ]
                        , el
                            [ centerY
                            , Events.onClick <| ClickedNameEdit
                            ]
                            (FeatherIcons.edit
                                |> FeatherIcons.toHtml []
                                |> html
                            )
                        ]

                    Editing _ mNewName ->
                        [ Input.text
                            Style.textInputStyle
                            { onChange = EditedName
                            , text = Maybe.withDefault "" mNewName
                            , placeholder = Nothing
                            , label =
                                Input.labelRight [ centerY ]
                                    (row [ spacing 5 ]
                                        [ Buttons.conditionalButton
                                            { label = text "Ok"
                                            , clickMsg = Just ClickedAcceptNameEdit
                                            , isActive =
                                                case mNewName of
                                                    Just newName ->
                                                        String.isEmpty newName
                                                            |> not

                                                    Nothing ->
                                                        False
                                            }
                                        , Buttons.alwaysActiveButton
                                            { label = text "Cancel"
                                            , clickMsg = ClickedCancelNameEdit
                                            , pressed = False
                                            }
                                        ]
                                    )
                            }
                        ]
                )
            , paragraph [] [ text ("Structure file: " ++ fileName) ]
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
                        [ basicMetrics designMetrics hoverInfoOption
                        , budeFFResultsTableView designMetrics hoverInfoOption
                        , evoEF2ResultsTableView evoEF2TableOption designMetrics displaySettings hoverInfoOption
                        , dfire2ResultsView designMetrics displaySettings hoverInfoOption
                        , rosettaResultsTableView designMetrics displaySettings hoverInfoOption
                        , aggrescan3dResultsTableView designMetrics displaySettings hoverInfoOption
                        , case mReferenceSet of
                            Just _ ->
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


basicMetrics : Metrics.DesignMetrics -> Tooltips.HoverInfoOption -> Element Msg
basicMetrics metrics hoverInfoOption =
    let
        { sequenceInfo } =
            metrics
    in
    sectionColumn
        [ Style.h2 <| text "Basic Metrics"
        , Style.h3 <| text "Sequences and DSSP Assignment"
        , sequenceInfoDictView sequenceInfo
        , paragraph [ Font.italic, Font.size 14 ] [ text "DSSP Key" ]
        , wrappedRow
            [ spacing 15
            , centerX
            , Font.size 14
            ]
            ([ "H = α-helix"
             , "B = isolated β-bridge"
             , "E = extended β-strand"
             , "G = 3-10 helix"
             , "I = π-helix"
             , "T = hydrogen-bonded turn"
             , "S = bend"
             , "- = loop"
             ]
                |> List.intersperse "|"
                |> List.map text
            )
        , metricsOverview metrics hoverInfoOption
        ]


metricsOverview : Metrics.DesignMetrics -> Tooltips.HoverInfoOption -> Element Msg
metricsOverview metrics hoverInfoOption =
    column [ spacing 10, width fill ]
        [ Style.h2 <| text "Metrics"
        , wrappedRow
            [ spacing 5
            , centerX
            ]
            [ el (Tooltips.hydrophobicFitnessHoverBox hoverInfoOption ChangeHoverInfo) <|
                createTableFloatColumn metrics.hydrophobicFitness 0 "Hydrophobic Fitness"
            , el (Tooltips.isoelectricPointHoverBox hoverInfoOption ChangeHoverInfo) <|
                createTableColumn cell (roundFloatText metrics.isoelectricPoint 0) "Isoelectric Point"
            , el (Tooltips.numOfResiduesHoverBox hoverInfoOption ChangeHoverInfo) <|
                createTableColumn cell (intText metrics.numOfResidues) "Number of Residues"
            , el (Tooltips.massHoverBox hoverInfoOption ChangeHoverInfo) <|
                createTableColumn cell (roundFloatText metrics.mass 0) "Mass (Da)"
            , el (Tooltips.packingDensityHoverBox hoverInfoOption ChangeHoverInfo) <|
                createTableColumn cell (roundFloatText metrics.packingDensity 0) "Mean Packing Density"
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


budeFFResultsTableView :
    Metrics.DesignMetrics
    -> Tooltips.HoverInfoOption
    -> Element Msg
budeFFResultsTableView metrics hoverInfoOption =
    sectionColumn
        [ Style.h3 <| text "BUDE Force Field Results"
        , wrappedRow
            [ spacing 5
            , centerX
            ]
            [ el (Tooltips.budeFFTotalEnergyHoverBox hoverInfoOption ChangeHoverInfo) <|
                createTableFloatColumn metrics.budeFFResults.totalEnergy 0 "Total Energy"
            , el (Tooltips.budeFFStericHoverBox hoverInfoOption ChangeHoverInfo) <|
                createTableFloatColumn metrics.budeFFResults.steric 0 "Steric"
            , el (Tooltips.budeFFDesolvationHoverBox hoverInfoOption ChangeHoverInfo) <|
                createTableFloatColumn metrics.budeFFResults.desolvation 0 "Desolvation"
            , el (Tooltips.budeFFChargeHoverBox hoverInfoOption ChangeHoverInfo) <|
                createTableFloatColumn metrics.budeFFResults.charge 0 "Charge"
            ]
        ]


evoEF2ResultsTableView :
    EvoEF2TableOption
    -> Metrics.DesignMetrics
    -> DisplaySettings
    -> Tooltips.HoverInfoOption
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
                    , label =
                        Input.labelAbove []
                            (paragraph [] [ text "Select the table view for the EvoEF2 results" ])
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
            [ spacing 5
            , centerX
            ]
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
    -> Tooltips.HoverInfoOption
    -> List (Element Msg)
evoef2SummaryColumns metrics hoverInfoOption =
    [ el (Tooltips.evoEF2SummaryTotalHoverBox hoverInfoOption ChangeHoverInfo) <|
        createTableFloatColumn metrics.evoEF2Results.total 0 "Total Energy"
    , el (Tooltips.evoEF2SummaryRefHoverBox hoverInfoOption ChangeHoverInfo) <|
        createTableFloatColumn metrics.evoEF2Results.ref_total 0 "Reference"
    , el (Tooltips.evoEF2SummaryIntraRHoverBox hoverInfoOption ChangeHoverInfo) <|
        createTableFloatColumn metrics.evoEF2Results.intraR_total 0 "Intra Residue"
    , el (Tooltips.evoEF2SummaryInterSHoverBox hoverInfoOption ChangeHoverInfo) <|
        createTableFloatColumn metrics.evoEF2Results.interS_total 0 "Inter Residue - Same Chain"
    , el (Tooltips.evoEF2SummaryInterDHoverBox hoverInfoOption ChangeHoverInfo) <|
        createTableFloatColumn metrics.evoEF2Results.interD_total 0 "Inter Residue - Different Chains"
    ]


evoef2RefColumns :
    Metrics.DesignMetrics
    -> Tooltips.HoverInfoOption
    -> List (Element Msg)
evoef2RefColumns metrics hoverInfoOption =
    [ el (Tooltips.evoEF2RefALAHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.reference_ALA 0 "ALA"
    , el (Tooltips.evoEF2RefARGHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.reference_ARG 0 "ARG"
    , el (Tooltips.evoEF2RefASNHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.reference_ASN 0 "ASN"
    , el (Tooltips.evoEF2RefASPHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.reference_ASP 0 "ASP"
    , el (Tooltips.evoEF2RefCYSHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.reference_CYS 0 "CYS"
    , el (Tooltips.evoEF2RefGLNHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.reference_GLN 0 "GLN"
    , el (Tooltips.evoEF2RefGLUHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.reference_GLU 0 "GLU"
    , el (Tooltips.evoEF2RefGLYHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.reference_GLY 0 "GLY"
    , el (Tooltips.evoEF2RefHISHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.reference_HIS 0 "HIS"
    , el (Tooltips.evoEF2RefILEHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.reference_ILE 0 "ILE"
    , el (Tooltips.evoEF2RefLEUHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.reference_LEU 0 "LEU"
    , el (Tooltips.evoEF2RefLYSHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.reference_LYS 0 "LYS"
    , el (Tooltips.evoEF2RefMETHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.reference_MET 0 "MET"
    , el (Tooltips.evoEF2RefPHEHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.reference_PHE 0 "PHE"
    , el (Tooltips.evoEF2RefPROHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.reference_PRO 0 "PRO"
    , el (Tooltips.evoEF2RefSERHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.reference_SER 0 "SER"
    , el (Tooltips.evoEF2RefTHRHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.reference_THR 0 "THR"
    , el (Tooltips.evoEF2RefTRPHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.reference_TRP 0 "TRP"
    , el (Tooltips.evoEF2RefTYRHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.reference_TYR 0 "TYR"
    , el (Tooltips.evoEF2RefVALHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.reference_VAL 0 "VAL"
    ]


evoef2IntraRColumns :
    Metrics.DesignMetrics
    -> Tooltips.HoverInfoOption
    -> List (Element Msg)
evoef2IntraRColumns metrics hoverInfoOption =
    [ el (Tooltips.evoEF2IntraRVDWAttHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.intraR_vdwatt 0 "VDW Attractive"
    , el (Tooltips.evoEF2IntraRVDWRepHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.intraR_vdwrep 0 "VDW Repulsive"
    , el (Tooltips.evoEF2IntraRElecHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.intraR_electr 0 "Electrostatics"
    , el (Tooltips.evoEF2IntraRDesolvPHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.intraR_deslvP 0 "Desolvation Polar"
    , el (Tooltips.evoEF2IntraRDesolvHHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.intraR_deslvH 0 "Desolvation Non Polar"
    , el (Tooltips.evoEF2IntraRAAPropHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.aapropensity 0 "Amino Acid Propensity"
    , el (Tooltips.evoEF2IntraRRamaHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.ramachandran 0 "Ramachandran"
    , el (Tooltips.evoEF2IntraRDunbrackHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.dunbrack 0 "Dunbrack Rotamer"
    , el (Tooltips.evoEF2IntraRHBSCBBDisHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.intraR_hbscbb_dis 0 "HB Sidechain Backbone Distance"
    , el (Tooltips.evoEF2IntraRHBSCBBTheHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.intraR_hbscbb_the 0 "HB Sidechain Backbone Theta"
    , el (Tooltips.evoEF2IntraRHBSCBBPhiHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.intraR_hbscbb_phi 0 "HB Sidechain Backbone Phi"
    ]


evoef2InterSColumns :
    Metrics.DesignMetrics
    -> Tooltips.HoverInfoOption
    -> List (Element Msg)
evoef2InterSColumns metrics hoverInfoOption =
    [ el (Tooltips.evoEF2InterSVDWAttHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.interS_vdwatt 0 "VDW Attractive"
    , el (Tooltips.evoEF2InterSVDWRepHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.interS_vdwrep 0 "VDW Repulsive"
    , el (Tooltips.evoEF2InterSElecHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.interS_electr 0 "Electrostatics"
    , el (Tooltips.evoEF2InterSDesolvPHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.interS_deslvP 0 "Desolvation Polar"
    , el (Tooltips.evoEF2InterSDesolvHHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.interS_deslvH 0 "Desolvation Non Polar"
    , el (Tooltips.evoEF2InterSSSbondHHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.interS_ssbond 0 "Disulfide Bonding"
    , el (Tooltips.evoEF2InterSHBBBBBDisHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.interS_hbbbbb_dis 0 "HB Backbone Backbone Distance"
    , el (Tooltips.evoEF2InterSHBBBBBTheHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.interS_hbbbbb_the 0 "HB Backbone Backbone Theta"
    , el (Tooltips.evoEF2InterSHBBBBBPhiHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.interS_hbbbbb_phi 0 "HB Backbone Backbone Phi"
    , el (Tooltips.evoEF2InterSHBSCBBDisHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.interS_hbscbb_dis 0 "HB Sidechain Backbone Distance"
    , el (Tooltips.evoEF2InterSHBSCBBTheHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.interS_hbscbb_the 0 "HB Sidechain Backbone Theta"
    , el (Tooltips.evoEF2InterSHBSCBBPhiHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.interS_hbscbb_phi 0 "HB Sidechain Backbone Phi"
    , el (Tooltips.evoEF2InterSHBSCSCDisHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.interS_hbscsc_dis 0 "HB Sidechain Sidechain Distance"
    , el (Tooltips.evoEF2InterSHBSCSCTheHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.interS_hbscsc_the 0 "HB Sidechain Sidechain Theta"
    , el (Tooltips.evoEF2InterSHBSCSCPhiHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.interS_hbscsc_phi 0 "HB Sidechain Sidechain Phi"
    ]


evoef2InterDColumns :
    Metrics.DesignMetrics
    -> Tooltips.HoverInfoOption
    -> List (Element Msg)
evoef2InterDColumns metrics hoverInfoOption =
    [ el (Tooltips.evoEF2InterDVDWAttHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.interD_vdwatt 0 "VDW Attractive"
    , el (Tooltips.evoEF2InterDVDWRepHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.interD_vdwrep 0 "VDW Repulsive"
    , el (Tooltips.evoEF2InterDElecHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.interD_electr 0 "Electrostatics"
    , el (Tooltips.evoEF2InterDDesolvPHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.interD_deslvP 0 "Desolvation Polar"
    , el (Tooltips.evoEF2InterDDesolvHHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.interD_deslvH 0 "Desolvation Non Polar"
    , el (Tooltips.evoEF2InterDSSbondHHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.interD_ssbond 0 "Disulfide Bonding"
    , el (Tooltips.evoEF2InterDHBBBBBDisHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.interD_hbbbbb_dis 0 "HB Backbone Backbone Distance"
    , el (Tooltips.evoEF2InterDHBBBBBTheHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.interD_hbbbbb_the 0 "HB Backbone Backbone Theta"
    , el (Tooltips.evoEF2InterDHBBBBBPhiHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.interD_hbbbbb_phi 0 "HB Backbone Backbone Phi"
    , el (Tooltips.evoEF2InterDHBSCBBDisHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.interD_hbscbb_dis 0 "HB Sidechain Backbone Distance"
    , el (Tooltips.evoEF2InterDHBSCBBTheHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.interD_hbscbb_the 0 "HB Sidechain Backbone Theta"
    , el (Tooltips.evoEF2InterDHBSCBBPhiHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.interD_hbscbb_phi 0 "HB Sidechain Backbone Phi"
    , el (Tooltips.evoEF2InterDHBSCSCDisHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.interD_hbscsc_dis 0 "HB Sidechain Sidechain Distance"
    , el (Tooltips.evoEF2InterDHBSCSCTheHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.interD_hbscsc_the 0 "HB Sidechain Sidechain Theta"
    , el (Tooltips.evoEF2InterDHBSCSCPhiHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.evoEF2Results.interD_hbscsc_phi 0 "HB Sidechain Sidechain Phi"
    ]


dfire2LogInfoSelection : Metrics.DesignMetrics -> String
dfire2LogInfoSelection metrics =
    case ( metrics.dfire2Results.return_code, metrics.dfire2Results.error_info ) of
        ( 0, "" ) ->
            metrics.dfire2Results.log_info

        _ ->
            metrics.dfire2Results.error_info


dfire2ResultsView :
    Metrics.DesignMetrics
    -> DisplaySettings
    -> Tooltips.HoverInfoOption
    -> Element Msg
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
            [ el (Tooltips.dfire2TotalHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.dfire2Results.total 0 "Total Energy" ]
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
    -> Tooltips.HoverInfoOption
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
            [ spacing 5
            , centerX
            ]
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
    -> Tooltips.HoverInfoOption
    -> List (Element Msg)
rosettaColumns metrics hoverInfoOption =
    [ el (Tooltips.rosettaTotalHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.rosettaResults.total_score 0 "Total Energy"
    , el (Tooltips.rosettaReferenceHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.rosettaResults.ref 0 "Reference"
    , el (Tooltips.rosettaVDWAttHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.rosettaResults.fa_atr 0 "VDW Attractive"
    , el (Tooltips.rosettaVDWRepHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.rosettaResults.fa_rep 0 "VDW Repulsive"
    , el (Tooltips.rosettaVDWRepIntraRHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.rosettaResults.fa_intra_rep 0 "VDW Repulsive Intra Residue"
    , el (Tooltips.rosettaElecHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.rosettaResults.fa_elec 0 "Electrostatics"
    , el (Tooltips.rosettaSolvIsoHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.rosettaResults.fa_sol 0 "Solvation Isotropic"
    , el (Tooltips.rosettaSolvAnisoHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.rosettaResults.lk_ball_wtd 0 "Solvation Anisotropic Polar Atoms"
    , el (Tooltips.rosettaSolvIsoIntraRHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.rosettaResults.fa_intra_sol_xover4 0 "Solvation Isotropic Intra Residue"
    , el (Tooltips.rosettaHBLRBBHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.rosettaResults.hbond_lr_bb 0 "HB Long Range Backbone"
    , el (Tooltips.rosettaHBSRBBHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.rosettaResults.hbond_sr_bb 0 "HB Short Range Backbone"
    , el (Tooltips.rosettaHBBBSCHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.rosettaResults.hbond_bb_sc 0 "HB Backbone Sidechain"
    , el (Tooltips.rosettaHBSCSCHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.rosettaResults.hbond_sc 0 "HB Sidechain Sidechain"
    , el (Tooltips.rosettaSSbondHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.rosettaResults.dslf_fa13 0 "Disulfide Bridges"
    , el (Tooltips.rosettaRamaHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.rosettaResults.rama_prepro 0 "Backbone Torsion Preference"
    , el (Tooltips.rosettaAAPropHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.rosettaResults.p_aa_pp 0 "Amino Acid Propensity"
    , el (Tooltips.rosettaDunbrackHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.rosettaResults.fa_dun 0 "Dunbrack Rotamer"
    , el (Tooltips.rosettaOmegaPenHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.rosettaResults.omega 0 "Omega Penalty"
    , el (Tooltips.rosettaOpenProPenHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.rosettaResults.pro_close 0 "Open Proline Penalty"
    , el (Tooltips.rosettaTyroPenHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.rosettaResults.yhh_planarity 0 "Tyrosine χ3 Dihedral Angle Penalty"
    ]


aggrescan3dResultsTableView :
    Metrics.DesignMetrics
    -> DisplaySettings
    -> Tooltips.HoverInfoOption
    -> Element Msg
aggrescan3dResultsTableView metrics displaySettings hoverInfoOption =
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
                [ text metrics.aggrescan3dResults.error_info
                ]
    in
    sectionColumn
        [ Style.h3 <| text "Aggrescan3D 2.0 - Aggregation Propensity Results"
        , wrappedRow
            [ spacing 5
            , centerX
            ]
            [ el (Tooltips.agg3dTotalScoreHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.aggrescan3dResults.total_value 2 "Total Score"
            , el (Tooltips.agg3dAvgScoreHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.aggrescan3dResults.avg_value 2 "Average Score"
            , el (Tooltips.agg3dMinScoreHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.aggrescan3dResults.min_value 2 "Minimum Score"
            , el (Tooltips.agg3dMaxScoreHoverBox hoverInfoOption ChangeHoverInfo) <| createTableFloatColumn metrics.aggrescan3dResults.max_value 2 "Maximum Score"
            ]
        , Folds.sectionFoldView
            { foldVisible = displaySettings.aggrescan3dLogInfo
            , title = hideableSectionToString Aggrescan3dLogInfo
            , toggleMsg = ToggleSectionVisibility Aggrescan3dLogInfo
            , contentView = logInfoBox
            }
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
            , width <| px 125
            , Background.color Style.colorPalette.c1
            , Font.center
            , Font.color Style.colorPalette.white
            ]
          <|
            paragraph [ centerY ] [ text metricName ]
        , el
            [ width fill
            , height <| px 40
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
intText value =
    text (String.fromInt value)


roundFloatText : Float -> Int -> Element msg
roundFloatText value digits =
    text (Round.round digits value)


createTableFloatColumn : Maybe Float -> Int -> String -> Element msg
createTableFloatColumn value digits colName =
    case value of
        Just a ->
            createTableColumn cell (roundFloatText a digits) colName

        Nothing ->
            createTableColumn cell (text "--") colName



--- }}}
--- {{{ specificationView


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



--- }}}
--- {{{ requirementView


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
                                "Value: "

                            requirementString =
                                case valueType of
                                    Requirement.IsoelectricPoint order value ->
                                        typeString
                                            ++ "Isoelectric Point: "
                                            ++ Requirement.stringFromOrder
                                                order
                                            ++ ": "
                                            ++ String.fromFloat value

                                    Requirement.HydrophobicFitness order value ->
                                        typeString
                                            ++ "Hydrophobic Fitness: "
                                            ++ Requirement.stringFromOrder
                                                order
                                            ++ ": "
                                            ++ String.fromFloat value

                                    Requirement.MeanPackingDensity order value ->
                                        typeString
                                            ++ "Mean Packing Density: "
                                            ++ Requirement.stringFromOrder
                                                order
                                            ++ ": "
                                            ++ String.fromFloat value

                                    Requirement.SequenceContains string ->
                                        typeString
                                            ++ "Sequence Contains: "
                                            ++ string

                                    Requirement.CompositionDeviation unitType value ->
                                        typeString
                                            ++ "Composition Deviation: "
                                            ++ Requirement.stringFromUnitType unitType
                                            ++ ": "
                                            ++ String.fromFloat value

                                    Requirement.BUDEFFTotal order value ->
                                        typeString
                                            ++ "BUDEFF Total Energy: "
                                            ++ Requirement.stringFromOrder
                                                order
                                            ++ ": "
                                            ++ String.fromFloat value

                                    Requirement.EvoEF2Total order value ->
                                        typeString
                                            ++ "EvoEF2 Total Energy: "
                                            ++ Requirement.stringFromOrder
                                                order
                                            ++ ": "
                                            ++ String.fromFloat value

                                    Requirement.DFIRE2Total order value ->
                                        typeString
                                            ++ "DFIRE2 Total Energy: "
                                            ++ Requirement.stringFromOrder
                                                order
                                            ++ ": "
                                            ++ String.fromFloat value

                                    Requirement.RosettaTotal order value ->
                                        typeString
                                            ++ "Rosetta Total Energy: "
                                            ++ Requirement.stringFromOrder
                                                order
                                            ++ ": "
                                            ++ String.fromFloat value

                                    Requirement.Agg3DTotal order value ->
                                        typeString
                                            ++ "Agg3D Total Score: "
                                            ++ Requirement.stringFromOrder
                                                order
                                            ++ ": "
                                            ++ String.fromFloat value
                        in
                        el (Style.defaultBorder ++ [ padding 10, width fill ])
                            (paragraph [] [ text <| requirementString ])

            Requirement.Not subRequirement ->
                row (Style.defaultBorder ++ [ padding 10, spacing 10, width fill ])
                    [ el [] <| Style.h3 <| el [ Font.bold ] (text <| "NOT")
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
