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
    , evoEF2Parameters : EvoEF2Params
    }


type PageState
    = AppNotRunning
    | LoadingNoStub
    | LoadingWithStub Design.DesignStub
    | UnknownDesignUuid
    | Design Design.Design


type EvoEF2Option
    = Summary
    | Reference
    | IntraR
    | InterS
    | InterD


type alias EvoEF2Columns =
    Metrics.DesignMetrics -> Element Msg


type alias EvoEF2Params =
    { evoEF2SelectedOption : EvoEF2Option
    , evoEF2SelectedColumns : EvoEF2Columns
    , evoEF2SelectedSubtitle : String
    }


type alias EvoEF2Data =
    { log_info : String
    , reference_ALA : Float
    , reference_CYS : Float
    , reference_ASP : Float
    , reference_GLU : Float
    , reference_PHE : Float
    , reference_GLY : Float
    , reference_HIS : Float
    , reference_ILE : Float
    , reference_LYS : Float
    , reference_LEU : Float
    , reference_MET : Float
    , reference_ASN : Float
    , reference_PRO : Float
    , reference_GLN : Float
    , reference_ARG : Float
    , reference_SER : Float
    , reference_THR : Float
    , reference_VAL : Float
    , reference_TRP : Float
    , reference_TYR : Float
    , intraR_vdwatt : Float
    , intraR_vdwrep : Float
    , intraR_electr : Float
    , intraR_deslvP : Float
    , intraR_deslvH : Float
    , intraR_hbscbb_dis : Float
    , intraR_hbscbb_the : Float
    , intraR_hbscbb_phi : Float
    , aapropensity : Float
    , ramachandran : Float
    , dunbrack : Float
    , interS_vdwatt : Float
    , interS_vdwrep : Float
    , interS_electr : Float
    , interS_deslvP : Float
    , interS_deslvH : Float
    , interS_ssbond : Float
    , interS_hbbbbb_dis : Float
    , interS_hbbbbb_the : Float
    , interS_hbbbbb_phi : Float
    , interS_hbscbb_dis : Float
    , interS_hbscbb_the : Float
    , interS_hbscbb_phi : Float
    , interS_hbscsc_dis : Float
    , interS_hbscsc_the : Float
    , interS_hbscsc_phi : Float
    , interD_vdwatt : Float
    , interD_vdwrep : Float
    , interD_electr : Float
    , interD_deslvP : Float
    , interD_deslvH : Float
    , interD_ssbond : Float
    , interD_hbbbbb_dis : Float
    , interD_hbbbbb_the : Float
    , interD_hbbbbb_phi : Float
    , interD_hbscbb_dis : Float
    , interD_hbscbb_the : Float
    , interD_hbscbb_phi : Float
    , interD_hbscsc_dis : Float
    , interD_hbscsc_the : Float
    , interD_hbscsc_phi : Float
    , total : Float

    -- , ref_total : Float
    -- , intraR_total : Float
    -- , interS_total : Float
    -- , interD_total : Float
    }



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
                    , evoEF2Parameters =
                        { evoEF2SelectedOption = Summary
                        , evoEF2SelectedColumns = evoef2SummaryColumns
                        , evoEF2SelectedSubtitle = "Summary"
                        }
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
              , evoEF2Parameters =
                    { evoEF2SelectedOption = Summary
                    , evoEF2SelectedColumns = evoef2SummaryColumns
                    , evoEF2SelectedSubtitle = "Summary"
                    }
              }
            , Cmd.none
            )



-- }}}
-- {{{ UPDATE


type Msg
    = SetFocus { uuidString : String, design : Value }
    | SetSpecification { uuidString : String, specValue : Value }
    | SetReferenceSet { uuidString : String, refSetValue : Value }
    | SetEvoEF2Option { option : EvoEF2Option }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetFocus { design } ->
            case Codec.decodeValue Design.codec design of
                Ok des ->
                    ( { model | pageState = Design des }
                    , Cmd.batch
                        [ Design.viewStructure des.pdbString
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

        SetEvoEF2Option option ->
            ( { model
                | evoEF2Parameters =
                    { evoEF2SelectedOption = option.option
                    , evoEF2SelectedColumns =
                        case option.option of
                            Summary ->
                                evoef2SummaryColumns

                            Reference ->
                                evoef2RefColumns

                            IntraR ->
                                evoef2IntraRColumns

                            InterS ->
                                evoef2InterSColumns

                            InterD ->
                                evoef2InterDColumns
                    , evoEF2SelectedSubtitle =
                        case option.option of
                            Summary ->
                                "Summary"

                            Reference ->
                                "Reference Energy Values"

                            IntraR ->
                                "IntraR Energy Values"

                            InterS ->
                                "InterS Energy Values"

                            InterD ->
                                "InterD Energy Values"
                    }
              }
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


createTableColumn metric metric_name =
    let
        onePlaceFloatText =
            Round.round 0
                >> text
                >> (\a -> cell a)

        tableColumn =
            column
                [ alignTop

                -- , width <| px 100
                ]

        cell =
            el [ centerX, padding 10 ]

        headerParagraph =
            paragraph
                [ padding 10
                , centerX
                , centerY
                , height <| px 90
                , Background.color Style.colorPalette.c1
                , Font.center
                , Font.color Style.colorPalette.white
                ]
    in
    tableColumn
        [ headerParagraph [ text metric_name ]
        , onePlaceFloatText metric
        ]


evoef2SummaryColumns : Metrics.DesignMetrics -> Element msg
evoef2SummaryColumns metrics =
    wrappedRow
        [ width fill, centerY, centerX ]
        [ createTableColumn metrics.evoEF2Results.total "Total EvoEF2 Energy"
        , createTableColumn metrics.evoEF2Results.aapropensity "AA Propensity Energy"
        , createTableColumn metrics.evoEF2Results.ramachandran "Ramachandran Energy"
        , createTableColumn metrics.evoEF2Results.dunbrack "Dunbrack Energy"

        -- , createTableColumn metrics.evoEF2Results.ref_total "Reference \nEnergy"
        -- , createTableColumn metrics.evoEF2Results.intraR_total "IntraR \nEnergy"
        -- , createTableColumn metrics.evoEF2Results.interS_total "InterS \nEnergy"
        -- , createTableColumn metrics.evoEF2Results.interD_total "InterD \nEnergy"
        ]


evoef2RefColumns : Metrics.DesignMetrics -> Element msg
evoef2RefColumns metrics =
    wrappedRow
        [ width fill, centerY, centerX ]
        [ createTableColumn metrics.evoEF2Results.reference_ALA "ALA"
        , createTableColumn metrics.evoEF2Results.reference_CYS "CYS"
        , createTableColumn metrics.evoEF2Results.reference_ASP "ASP"
        , createTableColumn metrics.evoEF2Results.reference_GLU "GLU"
        , createTableColumn metrics.evoEF2Results.reference_PHE "PHE"
        , createTableColumn metrics.evoEF2Results.reference_GLY "GLY"
        , createTableColumn metrics.evoEF2Results.reference_HIS "HIS"
        , createTableColumn metrics.evoEF2Results.reference_ILE "ILE"
        , createTableColumn metrics.evoEF2Results.reference_LYS "LYS"
        , createTableColumn metrics.evoEF2Results.reference_LEU "LEU"
        , createTableColumn metrics.evoEF2Results.reference_MET "MET"
        , createTableColumn metrics.evoEF2Results.reference_ASN "ASN"
        , createTableColumn metrics.evoEF2Results.reference_PRO "PRO"
        , createTableColumn metrics.evoEF2Results.reference_GLN "GLN"
        , createTableColumn metrics.evoEF2Results.reference_ARG "ARG"
        , createTableColumn metrics.evoEF2Results.reference_SER "SER"
        , createTableColumn metrics.evoEF2Results.reference_THR "THR"
        , createTableColumn metrics.evoEF2Results.reference_VAL "VAL"
        , createTableColumn metrics.evoEF2Results.reference_TRP "TRP"
        , createTableColumn metrics.evoEF2Results.reference_TYR "TYR"
        ]


evoef2IntraRColumns : Metrics.DesignMetrics -> Element msg
evoef2IntraRColumns metrics =
    wrappedRow
        [ width fill, centerY, centerX ]
        [ createTableColumn metrics.evoEF2Results.intraR_vdwatt "VDWATT"
        , createTableColumn metrics.evoEF2Results.intraR_vdwrep "VDWREP"
        , createTableColumn metrics.evoEF2Results.intraR_electr "ELECTR"
        , createTableColumn metrics.evoEF2Results.intraR_deslvP "DESLVP"
        , createTableColumn metrics.evoEF2Results.intraR_deslvH "DESLVH"
        , createTableColumn metrics.evoEF2Results.intraR_hbscbb_dis "HBSCBB DIS"
        , createTableColumn metrics.evoEF2Results.intraR_hbscbb_the "HBSCBB THE"
        , createTableColumn metrics.evoEF2Results.intraR_hbscbb_phi "HBSCBB PHI"
        ]


evoef2InterSColumns : Metrics.DesignMetrics -> Element msg
evoef2InterSColumns metrics =
    wrappedRow
        [ width fill, centerY, centerX ]
        [ createTableColumn metrics.evoEF2Results.interS_vdwatt "VDWATT"
        , createTableColumn metrics.evoEF2Results.interS_vdwrep "VDWREP"
        , createTableColumn metrics.evoEF2Results.interS_electr "ELECTR"
        , createTableColumn metrics.evoEF2Results.interS_deslvP "DESLVP"
        , createTableColumn metrics.evoEF2Results.interS_deslvH "DESLVH"
        , createTableColumn metrics.evoEF2Results.interS_hbbbbb_dis "HBBBBB DIS"
        , createTableColumn metrics.evoEF2Results.interS_hbbbbb_the "HBBBBB THE"
        , createTableColumn metrics.evoEF2Results.interS_hbbbbb_phi "HBBBBB PHI"
        , createTableColumn metrics.evoEF2Results.interS_hbscbb_dis "HBSCBB DIS"
        , createTableColumn metrics.evoEF2Results.interS_hbscbb_the "HBSCBB THE"
        , createTableColumn metrics.evoEF2Results.interS_hbscbb_phi "HBSCBB PHI"
        , createTableColumn metrics.evoEF2Results.interS_hbscsc_dis "HBSCSC DIS"
        , createTableColumn metrics.evoEF2Results.interS_hbscsc_the "HBSCSC THE"
        , createTableColumn metrics.evoEF2Results.interS_hbscsc_phi "HBSCSC PHI"
        ]


evoef2InterDColumns : Metrics.DesignMetrics -> Element msg
evoef2InterDColumns metrics =
    wrappedRow
        [ width fill, centerY, centerX ]
        [ createTableColumn metrics.evoEF2Results.interD_vdwatt "VDWATT"
        , createTableColumn metrics.evoEF2Results.interD_vdwrep "VDWREP"
        , createTableColumn metrics.evoEF2Results.interD_electr "ELECTR"
        , createTableColumn metrics.evoEF2Results.interD_deslvP "DESLVP"
        , createTableColumn metrics.evoEF2Results.interD_deslvH "DESLVH"
        , createTableColumn metrics.evoEF2Results.interD_hbbbbb_dis "HBBBBB DIS"
        , createTableColumn metrics.evoEF2Results.interD_hbbbbb_the "HBBBBB THE"
        , createTableColumn metrics.evoEF2Results.interD_hbbbbb_phi "HBBBBB PHI"
        , createTableColumn metrics.evoEF2Results.interD_hbscbb_dis "HBSCBB DIS"
        , createTableColumn metrics.evoEF2Results.interD_hbscbb_the "HBSCBB THE"
        , createTableColumn metrics.evoEF2Results.interD_hbscbb_phi "HBSCBB PHI"
        , createTableColumn metrics.evoEF2Results.interD_hbscsc_dis "HBSCSC DIS"
        , createTableColumn metrics.evoEF2Results.interD_hbscsc_the "HBSCSC THE"
        , createTableColumn metrics.evoEF2Results.interD_hbscsc_phi "HBSCSC PHI"
        ]


bodyView : Model -> Element Msg
bodyView { designUuid, pageState, mSelectedSpecification, mSelectedReferenceSet, evoEF2Parameters } =
    column [ width fill ]
        [ el [ centerX ] (Style.h1 <| text "Design Details")
        , case pageState of
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
                        , el [ Font.bold ] (text designUuid)
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
                        designUuid
                        (Maybe.andThen Stored.getData mSelectedSpecification)
                        (Maybe.andThen Stored.getData mSelectedReferenceSet)
                        design
                        evoEF2Parameters
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
    -> EvoEF2Params
    -> Element Msg
designDetailsView uuidString mSpecification mReferenceSet design evoEF2Parameters =
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
            , Keyed.el [ height <| px 400, width fill, padding 5, Border.width 1 ]
                ( "viewer"
                , Html.div
                    [ HAtt.id "viewer"
                    , HAtt.style "height" "100%"
                    , HAtt.style "width" "100%"
                    ]
                    []
                    |> html
                )
            ]
        ]
            ++ (case WebSockets.getDesignMetrics metricsJobStatus of
                    Just designMetrics ->
                        [ basicMetrics designMetrics
                        , evoEF2ResultsTableView evoEF2Parameters designMetrics
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
    let
        onePlaceFloatText =
            Round.round 1
                >> text
                >> (\a -> cell a)

        intText =
            String.fromInt >> text >> (\a -> cell a)

        cell =
            el [ centerX, padding 10 ]

        tableColumn =
            column
                [ alignTop
                , width <| px 150
                ]

        headerParagraph =
            paragraph
                [ padding 10
                , height <| px 70
                , Background.color Style.colorPalette.c1
                , Font.center
                , Font.color Style.colorPalette.white
                ]
    in
    column [ spacing 10, width fill ]
        [ Style.h2 <| text "Metrics"
        , wrappedRow
            [ centerX ]
            [ tableColumn
                [ headerParagraph [ text "Hydrophobic Fitness" ]
                , case metrics.hydrophobicFitness of
                    Just hf ->
                        onePlaceFloatText hf

                    Nothing ->
                        text "--"
                ]
            , tableColumn
                [ headerParagraph [ text "pI" ]
                , onePlaceFloatText metrics.isoelectricPoint
                ]
            , tableColumn
                [ headerParagraph [ text "# of Residues" ]
                , intText metrics.numOfResidues
                ]
            , tableColumn
                [ headerParagraph [ text "Mass (Da)" ]
                , onePlaceFloatText metrics.mass
                ]
            , tableColumn
                [ headerParagraph [ text "Mean Packing Density" ]
                , onePlaceFloatText metrics.packingDensity
                ]
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


changedSelected : EvoEF2Option -> Msg
changedSelected option =
    SetEvoEF2Option { option = option }


evoEF2ResultsTableView : EvoEF2Params -> Metrics.DesignMetrics -> Element Msg
evoEF2ResultsTableView evoEF2Params metrics =
    let
        radioInputSelection =
            el
                [ alignLeft
                , spacing 20
                , padding 20
                ]
            <|
                Input.radio
                    [ padding 20
                    , spacing 20
                    ]
                    { onChange = changedSelected
                    , selected = Just evoEF2Params.evoEF2SelectedOption
                    , label = Input.labelAbove [] (text "Select Table View")
                    , options =
                        [ Input.option Summary (text "Summary")
                        , Input.option Reference (text "Reference")
                        , Input.option IntraR (text "IntraR")
                        , Input.option InterS (text "InterS")
                        , Input.option InterD (text "InterD")
                        ]
                    }

        cell =
            el [ centerX, padding 10 ]

        logInfoBox =
            el
                [ centerX
                , spacing 20
                , padding 20
                , Border.rounded 1
                , Border.color (rgba 0 0 0 1)
                , Border.widthXY 2 2
                ]
            <|
                text metrics.evoEF2Results.log_info
    in
    sectionColumn
        [ Style.h3 <| text ("EvoEF2 Energy Function Results - " ++ evoEF2Params.evoEF2SelectedSubtitle)
        , row [ width fill, centerY, centerX, spacing 100 ]
            [ radioInputSelection
            , wrappedRow
                [ centerX, centerY ]
                [ evoEF2Params.evoEF2SelectedColumns metrics
                ]
            ]
        , cell (text "EvoEF2 Log Information")
        , logInfoBox
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
