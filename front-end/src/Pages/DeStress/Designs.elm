port module Pages.DeStress.Designs exposing (Model, Msg, Params, page)

import Biomolecules
import Browser.Navigation exposing (Key)
import Codec exposing (Value)
import Csv.Encode as CsvEncode
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import FeatherIcons
import File exposing (File)
import File.Select as FileSelect
import Html
import Html.Attributes as HAtt
import Set
import Shared
import Shared.Buttons as Buttons
import Shared.Design as Design exposing (Design)
import Shared.Editable as Editable
import Shared.Error as Error
import Shared.Metrics as Metrics
import Shared.Plots as Plots
import Shared.ResourceUuid as ResourceUuid exposing (ResourceUuid)
import Shared.Specification as Specification exposing (Specification)
import Shared.Style as Style
import Shared.WebSockets as WebSockets
import Spa.Document exposing (Document)
import Spa.Generated.Route as Route
import Spa.Page as Page exposing (Page)
import Spa.Url exposing (Url)
import Task
import Utils.Route exposing (navigate)
import VegaLite as VL


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


port createFile : String -> Cmd msg


port setSpecificationForDesign : (Value -> msg) -> Sub msg



-- }}}
-- {{{ MODEL


type alias Model =
    { mResourceUuid : Maybe ResourceUuid
    , loadingState : DesignLoadingState
    , pageErrors : List Error.Error
    , designs : Dict String Design.StoredDesign
    , mSelectedSpecification : Maybe Specification
    , deleteAllStatus : Buttons.DangerStatus
    , navKey : Key
    , selectedUuids : Set.Set String
    , tagString : String
    , filterTags : Set.Set String
    , device : Device
    , columnViewMode : ColumnViewMode
    , maxFilesInUploads : Int
    , maxResiduesInStructure : Int
    }


type DesignLoadingState
    = LoadingFiles Int Int
    | Free


type ColumnViewMode
    = DesignList
    | OverviewPlots
    | ControlPanel


columnViewModeToString : ColumnViewMode -> String
columnViewModeToString viewMode =
    case viewMode of
        DesignList ->
            "Design List"

        OverviewPlots ->
            "Overview Plots"

        ControlPanel ->
            "Control Panel"



-- }}}
-- {{{ INIT


type alias Params =
    ()


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared _ =
    let
        ( mResourceUuid, designDict ) =
            case Shared.getRunState shared of
                Just { resourceUuid, designs } ->
                    ( Just resourceUuid, designs )

                Nothing ->
                    ( Nothing, Dict.empty )

        model =
            { mResourceUuid = mResourceUuid
            , loadingState = Free
            , pageErrors = []
            , designs = designDict
            , mSelectedSpecification = Nothing
            , deleteAllStatus = Buttons.initDangerStatus
            , navKey = shared.key
            , selectedUuids = Set.empty
            , tagString = ""
            , filterTags = Set.empty
            , device = classifyDevice shared
            , columnViewMode = DesignList
            , maxFilesInUploads = 30
            , maxResiduesInStructure = 500
            }

        plotCmd =
            model.designs
                |> Dict.toList
                |> List.map (Tuple.mapSecond Design.storedDesignToStub)
                |> Dict.fromList
                |> makeOverViewSpec model.device
                |> Plots.vegaPlot
    in
    ( model
    , Cmd.batch
        [ case Shared.getRunState shared of
            Just runState ->
                case runState.mSelectedSpecification of
                    Just uuidString ->
                        Specification.getSpecificationForDesignsPage
                            { uuidString = uuidString }

                    Nothing ->
                        Cmd.none

            Nothing ->
                Cmd.none
        , case
            ( model.columnViewMode
            , model.device.class
            , model.device.orientation
            )
          of
            ( OverviewPlots, _, _ ) ->
                plotCmd

            ( _, Desktop, Landscape ) ->
                plotCmd

            ( _, BigDesktop, Landscape ) ->
                plotCmd

            _ ->
                Cmd.none
        ]
    )



-- }}}
-- {{{ UPDATE


type Msg
    = StructuresRequested
    | StructureFilesSelected File (List File)
    | StructureLoaded String String
    | GotSpecification Value
    | DeleteDesign String Buttons.DangerStatus
    | DeleteAllDesigns Buttons.DangerStatus
    | DesignDetails String
    | ClearPageErrors
    | SelectedDesign String Bool
    | UpdateTagString String
    | AddTags (Set.Set String) String
    | CancelSelection
    | UpdateFilterTags FilterTagsOption
    | ChangeColumnViewMode ColumnViewMode
    | ExportAllDesignData


type FilterTagsOption
    = AddOrRemove String
    | RemoveAll
    | AddAll


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StructuresRequested ->
            ( model, structureRequested )

        StructureFilesSelected first rest ->
            let
                cappedRest =
                    List.take (model.maxFilesInUploads - 1) rest

                loadedDesigns =
                    List.length cappedRest
                        |> (+) 1

                ( updatedModel, cmd ) =
                    if List.length rest > List.length cappedRest then
                        Error.updateWithError
                            ClearPageErrors
                            { model | loadingState = LoadingFiles loadedDesigns 0 }
                            { title = "Number of files exceeds limit"
                            , details =
                                """The maximum number of files that can be loaded at one
                                time is """
                                    ++ String.fromInt model.maxFilesInUploads
                                    ++ """. The first """
                                    ++ String.fromInt model.maxFilesInUploads
                                    ++ """ files will be loaded. Please load the rest in
                                    batches. This cap is in place to ensure server
                                    stability and a good user experience.
                                    """
                            , severity = Error.Low
                            }

                    else
                        ( { model | loadingState = LoadingFiles loadedDesigns 0 }
                        , Cmd.none
                        )
            in
            ( updatedModel
            , Cmd.batch <|
                cmd
                    :: List.map
                        (\file ->
                            Task.perform (StructureLoaded <| File.name file)
                                (File.toString file)
                        )
                        (first :: cappedRest)
            )

        StructureLoaded name contents ->
            let
                loadingState =
                    case model.loadingState of
                        LoadingFiles total remaining ->
                            let
                                updatedRemaining =
                                    remaining + 1
                            in
                            if updatedRemaining == total then
                                Free

                            else
                                LoadingFiles total updatedRemaining

                        Free ->
                            Free

                rStructuralData =
                    Biomolecules.parsePdbString name contents
            in
            case ( model.mResourceUuid, rStructuralData ) of
                ( Nothing, _ ) ->
                    Error.updateWithError
                        ClearPageErrors
                        { model | loadingState = loadingState }
                        { title = "Could not generate UUID"
                        , details =
                            """Please try refreshing your browser. If this issue
                                persists, please report it. See the front page for
                                information on reporting bugs.
                                """
                        , severity = Error.High
                        }

                ( _, Err (Biomolecules.PdbParseError errorString) ) ->
                    Error.updateWithError
                        ClearPageErrors
                        { model | loadingState = loadingState }
                        { title = "Failed to parse PDB file"
                        , details =
                            name
                                ++ ":\n\t"
                                ++ errorString
                        , severity = Error.Low
                        }

                ( _, Err (Biomolecules.HttpError _) ) ->
                    Error.updateWithError
                        ClearPageErrors
                        { model | loadingState = loadingState }
                        { title = "Failed to load file from server"
                        , details =
                            "Something weird happened while loading "
                                ++ name
                                ++ """ Please try refreshing your browser. If this
                             issue persists, please report it. See the front page
                             for information on reporting bugs.
                             """
                        , severity = Error.Low
                        }

                ( Just resourceUuid, Ok biomol ) ->
                    if
                        (Biomolecules.residues biomol.atoms
                            |> List.filter
                                (\{ residueName } ->
                                    String.toUpper
                                        residueName
                                        /= "HOH"
                                )
                            |> List.length
                        )
                            > model.maxResiduesInStructure
                    then
                        Error.updateWithError
                            ClearPageErrors
                            { model | loadingState = loadingState }
                            { title = "Structure exceeds size limit"
                            , details =
                                "A file ("
                                    ++ name
                                    ++ """) exceeds the maximum number of residues
                                    allowed in the application ("""
                                    ++ String.fromInt model.maxResiduesInStructure
                                    ++ """). This cap is in place to ensure server
                                    stability and a good user experience.
                                    """
                            , severity = Error.Low
                            }

                    else
                        let
                            { uuidString, nextResourceUuid } =
                                ResourceUuid.toString
                                    resourceUuid

                            pdbString =
                                contents
                                    |> String.lines
                                    |> List.filter (String.startsWith "ATOM")
                                    |> String.join "\n"

                            ( jobStatus, requestMetricsCmd ) =
                                WebSockets.prepareMetricsJob
                                    { uuidString = uuidString, pdbString = pdbString }

                            design : Design
                            design =
                                { name =
                                    String.split "." name
                                        |> List.head
                                        |> Maybe.withDefault name
                                        |> Editable.NotEditing
                                , fileName = name
                                , pdbString = pdbString
                                , deleteStatus = Buttons.initDangerStatus
                                , metricsJobStatus = jobStatus
                                , mMeetsActiveSpecification = Nothing
                                , tags = Set.empty
                                }
                        in
                        ( { model
                            | mResourceUuid = Just nextResourceUuid
                            , loadingState = loadingState
                            , designs =
                                Dict.insert uuidString
                                    (Design.createDesignStub design
                                        |> Design.storeDesignStubLocally
                                    )
                                    model.designs
                          }
                        , Cmd.batch
                            [ Design.storeDesign
                                { uuidString = uuidString
                                , design = design |> Codec.encoder Design.codec
                                }
                            , requestMetricsCmd
                            ]
                        )

        GotSpecification specificationValue ->
            let
                specWithUuidCodec =
                    Codec.object
                        (\uuidString specification ->
                            { uuidString = uuidString
                            , specification = specification
                            }
                        )
                        |> Codec.field "uuidString" .uuidString Codec.string
                        |> Codec.field "specification"
                            .specification
                            Specification.codec
                        |> Codec.buildObject
            in
            ( { model
                | mSelectedSpecification =
                    Codec.decodeValue specWithUuidCodec specificationValue
                        |> Result.toMaybe
                        |> Maybe.map .specification
              }
            , Cmd.none
            )

        DeleteDesign uuidString dangerStatus ->
            if Buttons.isConfirmed dangerStatus then
                let
                    updatedModel =
                        { model | designs = Dict.remove uuidString model.designs }
                in
                ( updatedModel
                , Design.deleteDesign { uuidString = uuidString }
                )

            else
                let
                    updatedDesigns =
                        Dict.update
                            uuidString
                            ((\des ->
                                { des | deleteStatus = dangerStatus }
                             )
                                |> Design.mapStoredDesign
                                |> Maybe.map
                            )
                            model.designs
                in
                ( { model | designs = updatedDesigns }
                , Cmd.none
                )

        DeleteAllDesigns dangerStatus ->
            if Buttons.isConfirmed dangerStatus then
                let
                    updatedModel =
                        { model
                            | deleteAllStatus = Buttons.initDangerStatus
                            , designs = Dict.empty
                        }
                in
                ( updatedModel
                , Design.deleteAllDesigns ()
                )

            else
                ( { model | deleteAllStatus = dangerStatus }
                , Cmd.none
                )

        DesignDetails uuid ->
            ( model
            , navigate
                model.navKey
                (Route.DeStress__Designs__Uuid_String { uuid = uuid })
            )

        ClearPageErrors ->
            ( { model | pageErrors = [] }, Cmd.none )

        SelectedDesign uuid selected ->
            ( if selected then
                { model | selectedUuids = Set.insert uuid model.selectedUuids }

              else
                { model | selectedUuids = Set.remove uuid model.selectedUuids }
            , Cmd.none
            )

        UpdateTagString tagString ->
            ( { model | tagString = tagString }
            , Cmd.none
            )

        AddTags selectedUuids tagString ->
            let
                newTags =
                    stringToTags tagString
            in
            ( { model
                | designs =
                    Dict.map
                        (\k storedDesign ->
                            if Set.member k selectedUuids then
                                Design.mapStoredDesign
                                    (\d -> { d | tags = Set.union d.tags newTags })
                                    storedDesign

                            else
                                storedDesign
                        )
                        model.designs
                , selectedUuids = Set.empty
                , tagString = ""
              }
            , Cmd.none
            )

        CancelSelection ->
            ( { model
                | selectedUuids = Set.empty
                , tagString = ""
              }
            , Cmd.none
            )

        UpdateFilterTags option ->
            ( { model
                | filterTags =
                    case option of
                        AddOrRemove tag ->
                            if Set.member tag model.filterTags then
                                Set.remove tag model.filterTags

                            else
                                Set.insert tag model.filterTags

                        RemoveAll ->
                            Set.empty

                        AddAll ->
                            getAllTags model.designs
              }
            , Cmd.none
            )

        ChangeColumnViewMode newMode ->
            ( { model | columnViewMode = newMode }
            , case newMode of
                OverviewPlots ->
                    model.designs
                        |> Dict.toList
                        |> List.map (Tuple.mapSecond Design.storedDesignToStub)
                        |> Dict.fromList
                        |> makeOverViewSpec model.device
                        |> Plots.vegaPlot

                _ ->
                    Cmd.none
            )

        ExportAllDesignData ->
            let
                designStubs : List Design.DesignStub
                designStubs =
                    model.designs
                        |> Dict.values
                        |> List.map Design.storedDesignToStub

                noDataDesigns : List String
                noDataDesigns =
                    designStubs
                        |> List.filterMap
                            (\stub ->
                                if WebSockets.metricsAvailable stub.metricsJobStatus then
                                    Nothing

                                else
                                    Just stub.name
                            )

                csvString : String
                csvString =
                    designStubs
                        |> CsvEncode.encode
                            { encoder =
                                CsvEncode.withFieldNames
                                    designStubCSVEncoder
                            , fieldSeparator = ','
                            }

                cmds =
                    [ createFile csvString ]
            in
            if List.isEmpty noDataDesigns then
                ( model, Cmd.batch cmds )

            else
                Error.updateWithError
                    ClearPageErrors
                    model
                    { title = "Metrics not available"
                    , details =
                        noDataDesigns
                            |> List.intersperse ", "
                            |> String.concat
                            |> (++)
                                """The following designs do not have metrics
                                    available, so will not be included in the data
                                    export:
                                    """
                    , severity = Error.Low
                    }
                    |> Tuple.mapSecond (\c -> c :: cmds |> Cmd.batch)


designStubCSVEncoder : Design.DesignStub -> List ( String, String )
designStubCSVEncoder designStub =
    let
        header =
            [ ( "design name", designStub.name )
            , ( "file name", designStub.fileName )
            , ( "tags", String.join ":" (Set.toList designStub.tags) )
            ]

        mMetrics =
            WebSockets.getDesignMetrics designStub.metricsJobStatus

        createCompLine : Dict String Float -> String -> ( String, String )
        createCompLine compDict label =
            ( "composition: " ++ label
            , Dict.get label compDict
                |> Maybe.withDefault 0.0
                |> String.fromFloat
            )
    in
    header
        ++ (case mMetrics of
                Just metrics ->
                    ("ACDEFGHIKLMNPQRSTVWXY"
                        |> String.toList
                        |> List.map String.fromChar
                        |> List.map (createCompLine metrics.composition)
                    )
                        ++ [ ( "hydrophobic fitness"
                             , Maybe.map String.fromFloat metrics.hydrophobicFitness
                                |> Maybe.withDefault "NaN"
                             )
                           , ( "isoelectric point (pH)"
                             , String.fromFloat metrics.isoelectricPoint
                             )
                           , ( "mass (da)"
                             , String.fromFloat metrics.mass
                             )
                           , ( "number of residues"
                             , String.fromInt metrics.numOfResidues
                             )
                           , ( "packing density"
                             , String.fromFloat metrics.packingDensity
                             )

                           -- BUDE FF
                           , ( "budeff: total"
                             , Maybe.map String.fromFloat
                                metrics.budeFFResults.totalEnergy
                                |> Maybe.withDefault "NaN"
                             )
                           , ( "budeff: steric"
                             , Maybe.map String.fromFloat
                                metrics.budeFFResults.steric
                                |> Maybe.withDefault "NaN"
                             )
                           , ( "budeff: desolvation"
                             , Maybe.map String.fromFloat
                                metrics.budeFFResults.desolvation
                                |> Maybe.withDefault "NaN"
                             )
                           , ( "budeff: charge"
                             , Maybe.map String.fromFloat
                                metrics.budeFFResults.charge
                                |> Maybe.withDefault "NaN"
                             )

                           -- EvoEF2
                           , ( "evoef2: total"
                             , Maybe.map String.fromFloat
                                metrics.evoEF2Results.total
                                |> Maybe.withDefault "NaN"
                             )
                           , ( "evoef2: ref total"
                             , Maybe.map String.fromFloat
                                metrics.evoEF2Results.ref_total
                                |> Maybe.withDefault "NaN"
                             )
                           , ( "evoef2: intraR total"
                             , Maybe.map String.fromFloat
                                metrics.evoEF2Results.intraR_total
                                |> Maybe.withDefault "NaN"
                             )
                           , ( "evoef2: interS total"
                             , Maybe.map String.fromFloat
                                metrics.evoEF2Results.interS_total
                                |> Maybe.withDefault "NaN"
                             )
                           , ( "evoef2 - interD total"
                             , Maybe.map String.fromFloat
                                metrics.evoEF2Results.interD_total
                                |> Maybe.withDefault "NaN"
                             )

                           -- DFIRE2
                           , ( "dfire2 - total"
                             , Maybe.map String.fromFloat
                                metrics.dfire2Results.total
                                |> Maybe.withDefault "NaN"
                             )

                           -- Rosetta
                           , ( "rosetta - total"
                             , Maybe.map String.fromFloat
                                metrics.rosettaResults.total_score
                                |> Maybe.withDefault "NaN"
                             )
                           , ( "rosetta - fa_atr"
                             , Maybe.map String.fromFloat
                                metrics.rosettaResults.fa_atr
                                |> Maybe.withDefault "NaN"
                             )
                           , ( "rosetta - fa_rep"
                             , Maybe.map String.fromFloat
                                metrics.rosettaResults.fa_rep
                                |> Maybe.withDefault "NaN"
                             )
                           , ( "rosetta - fa_intra_rep"
                             , Maybe.map String.fromFloat
                                metrics.rosettaResults.fa_intra_rep
                                |> Maybe.withDefault "NaN"
                             )
                           , ( "rosetta - fa_elec"
                             , Maybe.map String.fromFloat
                                metrics.rosettaResults.fa_elec
                                |> Maybe.withDefault "NaN"
                             )
                           , ( "rosetta - fa_sol"
                             , Maybe.map String.fromFloat
                                metrics.rosettaResults.fa_sol
                                |> Maybe.withDefault "NaN"
                             )
                           , ( "rosetta - lk_ball_wtd"
                             , Maybe.map String.fromFloat
                                metrics.rosettaResults.lk_ball_wtd
                                |> Maybe.withDefault "NaN"
                             )
                           , ( "rosetta - fa_intra_sol_xover4"
                             , Maybe.map String.fromFloat
                                metrics.rosettaResults.fa_intra_sol_xover4
                                |> Maybe.withDefault "NaN"
                             )
                           , ( "rosetta - hbond_lr_bb"
                             , Maybe.map String.fromFloat
                                metrics.rosettaResults.hbond_lr_bb
                                |> Maybe.withDefault "NaN"
                             )
                           , ( "rosetta - hbond_sr_bb"
                             , Maybe.map String.fromFloat
                                metrics.rosettaResults.hbond_sr_bb
                                |> Maybe.withDefault "NaN"
                             )
                           , ( "rosetta - hbond_bb_sc"
                             , Maybe.map String.fromFloat
                                metrics.rosettaResults.hbond_bb_sc
                                |> Maybe.withDefault "NaN"
                             )
                           , ( "rosetta - hbond_sc"
                             , Maybe.map String.fromFloat
                                metrics.rosettaResults.hbond_sc
                                |> Maybe.withDefault "NaN"
                             )
                           , ( "rosetta - dslf_fa13"
                             , Maybe.map String.fromFloat
                                metrics.rosettaResults.dslf_fa13
                                |> Maybe.withDefault "NaN"
                             )
                           , ( "rosetta - rama_prepro"
                             , Maybe.map String.fromFloat
                                metrics.rosettaResults.rama_prepro
                                |> Maybe.withDefault "NaN"
                             )
                           , ( "rosetta - p_aa_pp"
                             , Maybe.map String.fromFloat
                                metrics.rosettaResults.p_aa_pp
                                |> Maybe.withDefault "NaN"
                             )
                           , ( "rosetta - fa_dun"
                             , Maybe.map String.fromFloat
                                metrics.rosettaResults.fa_dun
                                |> Maybe.withDefault "NaN"
                             )
                           , ( "rosetta - omega"
                             , Maybe.map String.fromFloat
                                metrics.rosettaResults.omega
                                |> Maybe.withDefault "NaN"
                             )
                           , ( "rosetta - pro_close"
                             , Maybe.map String.fromFloat
                                metrics.rosettaResults.pro_close
                                |> Maybe.withDefault "NaN"
                             )
                           , ( "rosetta - yhh_planarity"
                             , Maybe.map String.fromFloat
                                metrics.rosettaResults.yhh_planarity
                                |> Maybe.withDefault "NaN"
                             )

                           -- Aggrescan3D
                           , ( "aggrescan3d: total_value"
                             , Maybe.map String.fromFloat
                                metrics.aggrescan3dResults.total_value
                                |> Maybe.withDefault "NaN"
                             )
                           , ( "aggrescan3d: avg_value"
                             , Maybe.map String.fromFloat
                                metrics.aggrescan3dResults.avg_value
                                |> Maybe.withDefault "NaN"
                             )
                           , ( "aggrescan3d: min_value"
                             , Maybe.map String.fromFloat
                                metrics.aggrescan3dResults.min_value
                                |> Maybe.withDefault "NaN"
                             )
                           , ( "aggrescan3d: max_value"
                             , Maybe.map String.fromFloat
                                metrics.aggrescan3dResults.max_value
                                |> Maybe.withDefault "NaN"
                             )
                           ]

                Nothing ->
                    []
           )


structureRequested : Cmd Msg
structureRequested =
    FileSelect.files [ "*/*" ] StructureFilesSelected


save : Model -> Shared.Model -> Shared.Model
save model shared =
    let
        updatedShared =
            Error.updateSharedModelErrors model shared
    in
    case model.mResourceUuid of
        Just resourceUuid ->
            Shared.mapRunState
                (\runState ->
                    { runState
                        | resourceUuid = resourceUuid
                        , designs = model.designs
                        , saveStateRequested = True
                    }
                )
                updatedShared

        Nothing ->
            updatedShared


load : Shared.Model -> Model -> ( Model, Cmd Msg )
load shared model =
    case Shared.getRunState shared of
        Just runState ->
            let
                updatedModel =
                    { model
                        | mResourceUuid = Just runState.resourceUuid
                        , designs = runState.designs
                        , device = classifyDevice shared
                    }

                plotCmd =
                    updatedModel.designs
                        |> Dict.toList
                        |> List.map (Tuple.mapSecond Design.storedDesignToStub)
                        |> Dict.fromList
                        |> makeOverViewSpec updatedModel.device
                        |> Plots.vegaPlot
            in
            ( updatedModel
            , case
                ( updatedModel.columnViewMode
                , updatedModel.device.class
                , updatedModel.device.orientation
                )
              of
                ( OverviewPlots, _, _ ) ->
                    plotCmd

                ( _, Desktop, Landscape ) ->
                    plotCmd

                ( _, BigDesktop, Landscape ) ->
                    plotCmd

                _ ->
                    Cmd.none
            )

        Nothing ->
            Error.updateWithError
                ClearPageErrors
                model
                { title = "Failed to launch application"
                , details =
                    """Something has went wrong and the application has failed to
                        initialise. Please try refreshing your browser. If this error
                        persists, please submit a bug report. See the front page for
                        information on how to contact the authors."""
                , severity = Error.High
                }


subscriptions : Model -> Sub Msg
subscriptions _ =
    setSpecificationForDesign GotSpecification



-- }}}
-- {{{ VIEW


view : Model -> Document Msg
view model =
    { title = "Designs"
    , body = [ bodyView model ]
    }


bodyView : Model -> Element Msg
bodyView model =
    let
        designCardData =
            model.designs
                |> Dict.toList
                |> List.map
                    (\( k, v ) ->
                        ( k, Design.storedDesignToStub v )
                    )
                |> List.map
                    (createDesignCardData
                        Nothing
                        -- (mSelectedReferenceSet
                        --     |> Maybe.andThen
                        --         (\k -> Dict.get k referenceSets)
                        --     |> Maybe.map
                        --         (Global.storedReferenceSetToStub
                        --             >> ReferenceSet.getParamsForStub
                        --             >> .aggregateData
                        --         )
                        -- )
                        model.mSelectedSpecification
                        model.selectedUuids
                    )
    in
    el
        [ centerX
        , case ( model.device.class, model.device.orientation ) of
            ( Desktop, Landscape ) ->
                width <| maximum 1200 <| fill

            ( BigDesktop, Landscape ) ->
                width <| maximum 1200 <| fill

            _ ->
                width <| maximum 800 <| fill
        ]
    <|
        column [ spacing 15, width fill ]
            [ let
                ( buttonLabel, isActive ) =
                    case model.loadingState of
                        LoadingFiles total remaining ->
                            ( "Loaded "
                                ++ String.fromInt remaining
                                ++ "/"
                                ++ String.fromInt total
                            , False
                            )

                        Free ->
                            ( "Load", True )
              in
              wrappedRow [ centerX, spacing 10 ]
                [ paragraph [] [ Style.h1 <| text "Designs" ]
                , row [ spacing 10 ]
                    [ Buttons.conditionalButton
                        { label = text buttonLabel
                        , clickMsg = Just StructuresRequested
                        , isActive = isActive
                        }
                    , Buttons.dangerousButton
                        { label = text "Delete All"
                        , confirmText = "Are you sure you want to delete ALL design?"
                        , status = model.deleteAllStatus
                        , dangerousMsg = DeleteAllDesigns
                        }
                    ]
                ]
            , el [ width fill ] <|
                if List.isEmpty designCardData then
                    el [ centerX ] <|
                        paragraph []
                            [ text
                                """Click "Load" to add protein-structure models (in PDB
                                format)."""
                            ]

                else
                    case ( model.device.class, model.device.orientation ) of
                        ( Desktop, Landscape ) ->
                            doubleColumnView model designCardData

                        ( BigDesktop, Landscape ) ->
                            doubleColumnView model designCardData

                        _ ->
                            singleColumnView model designCardData
            ]


columnViewModeSelector : ColumnViewMode -> ColumnViewMode -> Element Msg
columnViewModeSelector current option =
    columnViewModeToString option
        |> text
        |> el
            ((if current == option then
                [ Font.underline ]

              else
                [ Events.onClick <| ChangeColumnViewMode option ]
             )
                ++ [ pointer ]
            )


singleColumnView : Model -> List DesignCardData -> Element Msg
singleColumnView model designCardData =
    column
        [ spacing 10, width fill ]
        [ row Style.scrollOptions <|
            (List.map
                (columnViewModeSelector model.columnViewMode)
                [ DesignList
                , OverviewPlots
                , ControlPanel
                ]
                |> List.intersperse (text "|")
            )
        , case model.columnViewMode of
            DesignList ->
                column
                    [ spacing 15, width fill ]
                    [ if Set.isEmpty model.selectedUuids then
                        none

                      else
                        selectedCommandsView model.selectedUuids model.tagString
                    , designCardsView
                        model.filterTags
                        model.mSelectedSpecification
                        designCardData
                    ]

            OverviewPlots ->
                overviewPlots

            ControlPanel ->
                controlPanel model
        ]


doubleColumnView : Model -> List DesignCardData -> Element Msg
doubleColumnView model designCardData =
    row
        [ width fill ]
        [ column
            [ alignTop, spacing 15, width <| fillPortion 1 ]
            [ row Style.scrollOptions <|
                (List.map
                    (columnViewModeSelector model.columnViewMode)
                    [ DesignList
                    , ControlPanel
                    ]
                    |> List.intersperse (text "|")
                )
            , case model.columnViewMode of
                ControlPanel ->
                    controlPanel model

                _ ->
                    column
                        [ spacing 15, width fill ]
                        [ if Set.isEmpty model.selectedUuids then
                            none

                          else
                            selectedCommandsView model.selectedUuids model.tagString
                        , designCardsView
                            model.filterTags
                            model.mSelectedSpecification
                            designCardData
                        ]
            ]
        , column [ alignTop, spacing 15, width <| fillPortion 2 ]
            [ el [ centerX ] <| Style.h2 <| text "Overview Plots"
            , overviewPlots
            ]
        ]



-- {{{ Design Cards


type alias DesignCardData =
    { uuidString : String
    , designStub : Design.DesignStub
    , mMeetsSpecification :
        Maybe Bool
    , selected : Bool
    }


createDesignCardData :
    Maybe Metrics.AggregateData
    -> Maybe Specification
    -> Set.Set String
    -> ( String, Design.DesignStub )
    -> DesignCardData
createDesignCardData mAggregateData mSpecification selectedUuids ( uuidString, designStub ) =
    { uuidString = uuidString
    , designStub = designStub
    , mMeetsSpecification =
        case ( mSpecification, WebSockets.getDesignMetrics designStub.metricsJobStatus ) of
            ( Just specification, Just designMetrics ) ->
                Specification.applySpecification mAggregateData
                    designMetrics
                    specification
                    |> Just

            _ ->
                Nothing
    , selected = Set.member uuidString selectedUuids
    }


partitionDesignCardData :
    List DesignCardData
    ->
        { meetsSpecification : List DesignCardData
        , failedSpecification : List DesignCardData
        , noMetrics : List DesignCardData
        }
    ->
        { meetsSpecification : List DesignCardData
        , failedSpecification : List DesignCardData
        , noMetrics : List DesignCardData
        }
partitionDesignCardData remainingData partitionedData =
    case remainingData of
        [] ->
            partitionedData

        data :: rest ->
            let
                newPartitioned =
                    case data.mMeetsSpecification of
                        Just True ->
                            { partitionedData
                                | meetsSpecification =
                                    data :: partitionedData.meetsSpecification
                            }

                        Just False ->
                            { partitionedData
                                | failedSpecification =
                                    data :: partitionedData.failedSpecification
                            }

                        Nothing ->
                            { partitionedData
                                | noMetrics =
                                    data :: partitionedData.noMetrics
                            }
            in
            partitionDesignCardData rest newPartitioned


designCardsView : Set.Set String -> Maybe Specification -> List DesignCardData -> Element Msg
designCardsView filterTags mSelectedSpecification allDesignCardData =
    let
        cardContainer =
            column [ spacing 10, width fill ]

        designCardData =
            List.filter
                (\{ designStub } ->
                    if Set.isEmpty filterTags then
                        True

                    else if Set.isEmpty designStub.tags then
                        True

                    else
                        Set.diff designStub.tags filterTags
                            |> Set.isEmpty
                            |> not
                )
                allDesignCardData

        numOfRunning =
            List.filter
                (\{ designStub } ->
                    WebSockets.isRunning designStub.metricsJobStatus
                )
                allDesignCardData
                |> List.length

        numOfDesigns =
            List.length allDesignCardData

        metricsProgress =
            if numOfRunning == 0 then
                none

            else
                Style.progressBar
                    { max = numOfDesigns
                    , current = numOfDesigns - numOfRunning
                    }
    in
    case mSelectedSpecification of
        Just _ ->
            let
                { meetsSpecification, failedSpecification, noMetrics } =
                    partitionDesignCardData designCardData
                        { meetsSpecification = []
                        , failedSpecification = []
                        , noMetrics = []
                        }
            in
            column [ spacing 15, width fill ]
                ([ metricsProgress
                 , Style.h2 <| text "Meets Specification"
                 , meetsSpecification
                    |> List.map designCardView
                    |> cardContainer
                 , Style.h2 <| text "Failed to Meet Specification"
                 , failedSpecification
                    |> List.map designCardView
                    |> cardContainer
                 ]
                    ++ (if List.isEmpty noMetrics then
                            []

                        else
                            [ Style.h2 <| text "No Metrics Available"
                            , noMetrics
                                |> List.map designCardView
                                |> cardContainer
                            ]
                       )
                )

        Nothing ->
            cardContainer
                (metricsProgress
                    :: (designCardData
                            |> List.map designCardView
                       )
                )


designCardView :
    { uuidString : String
    , designStub : Design.DesignStub
    , mMeetsSpecification :
        Maybe Bool
    , selected : Bool
    }
    -> Element Msg
designCardView { uuidString, designStub, mMeetsSpecification, selected } =
    row
        [ mouseOver [ Background.color Style.colorPalette.c4 ]
        , spacing 10
        , width fill
        , Background.color Style.colorPalette.c5
        , case mMeetsSpecification of
            Nothing ->
                Border.color Style.colorPalette.c5

            Just False ->
                Border.color Style.colorPalette.red

            Just True ->
                Border.color Style.colorPalette.c3
        , Border.width 4
        , Border.rounded 10
        ]
        [ Input.checkbox
            [ alignLeft
            , padding 5
            , width <| px 10
            ]
            { onChange = SelectedDesign uuidString
            , icon = Input.defaultCheckbox
            , checked = selected
            , label = Input.labelHidden "Select Design"
            }
        , column
            [ padding 10
            , spacing 5
            , width fill
            ]
            [ column
                [ pointer
                , width fill
                , DesignDetails uuidString
                    |> Events.onClick
                ]
                [ Style.h2 <| text designStub.name
                , paragraph []
                    [ WebSockets.metricsJobStatusString designStub.metricsJobStatus
                        |> text
                    ]
                ]
            , if Set.isEmpty designStub.tags then
                none

              else
                wrappedRow [ spacing 5 ]
                    (text "Tags:"
                        :: (Set.toList designStub.tags
                                |> List.map (tagView Nothing Set.empty)
                           )
                    )
            ]
        , el [ alignRight, padding 10 ] <|
            Buttons.dangerousButton
                { label = Style.featherIconToElmUi FeatherIcons.trash2
                , confirmText = "Are you sure you want to delete this design?"
                , status = designStub.deleteStatus
                , dangerousMsg = DeleteDesign uuidString
                }
        ]



-- }}}
-- {{{ Control Panel


controlPanel : Model -> Element Msg
controlPanel model =
    let
        allTags =
            getAllTags model.designs
    in
    column [ spacing 10 ]
        [ Style.h2 <| text "Export Design Data"
        , wrappedRow []
            [ Buttons.conditionalButton
                { label = text "Export All"
                , clickMsg = Just ExportAllDesignData
                , isActive =
                    case model.loadingState of
                        LoadingFiles _ _ ->
                            False

                        Free ->
                            True
                }
            ]
        , Style.h2 <| text "Tags"
        , if Set.isEmpty allTags then
            paragraph []
                [ text <|
                    ("No designs are tagged, select designs in order to tag them. "
                        ++ "This will help you keep your designs organised."
                    )
                ]

          else
            allTagsView
                { tags = getAllTags model.designs
                , filterTags = model.filterTags
                }
        ]



-- }}}
-- {{{ Tags


allTagsView : { tags : Set.Set String, filterTags : Set.Set String } -> Element Msg
allTagsView { tags, filterTags } =
    tags
        |> Set.toList
        |> List.map (tagView (Just (\ts -> AddOrRemove ts |> UpdateFilterTags)) filterTags)
        |> (++)
            [ text "Filter by Tag"
            , filterNoneTag { filterTags = filterTags, allTags = tags }
            , filterAllTag { filterTags = filterTags, allTags = tags }
            , text "|"
            ]
        |> wrappedRow [ spacing 10 ]


filterNoneTag : { filterTags : Set.Set String, allTags : Set.Set String } -> Element Msg
filterNoneTag { filterTags } =
    el
        [ padding 5
        , pointer
        , if Set.isEmpty filterTags then
            Background.color Style.colorPalette.c3

          else
            Background.color Style.colorPalette.c4
        , Border.rounded 8
        , Events.onClick (RemoveAll |> UpdateFilterTags)
        ]
    <|
        text "None"


filterAllTag : { filterTags : Set.Set String, allTags : Set.Set String } -> Element Msg
filterAllTag { filterTags, allTags } =
    el
        [ padding 5
        , pointer
        , if filterTags == allTags then
            Background.color Style.colorPalette.c3

          else
            Background.color Style.colorPalette.c4
        , Border.rounded 8
        , Events.onClick (AddAll |> UpdateFilterTags)
        ]
    <|
        text "All"


tagView : Maybe (String -> Msg) -> Set.Set String -> String -> Element Msg
tagView mMsg filterTags tag =
    el
        ([ padding 5
         , pointer
         , if Set.member tag filterTags then
            Background.color Style.colorPalette.c4

           else
            Background.color Style.colorPalette.c3
         , Border.rounded 8
         ]
            ++ (case mMsg of
                    Just msg ->
                        [ Events.onClick (msg tag) ]

                    Nothing ->
                        []
               )
        )
    <|
        text tag


selectedCommandsView : Set.Set String -> String -> Element Msg
selectedCommandsView selectedUuids tagString =
    wrappedRow [ spacing 10, width fill ]
        [ Input.text []
            { onChange = UpdateTagString
            , text = tagString
            , placeholder =
                Just
                    (Input.placeholder [] <|
                        text "tag1, tag2..."
                    )
            , label = Input.labelLeft [] <| text "Tags"
            }
        , Buttons.conditionalButton
            { label = text "Ok"
            , clickMsg = Just <| AddTags selectedUuids tagString
            , isActive =
                stringToTags tagString
                    |> Set.isEmpty
                    |> not
            }
        , Buttons.alwaysActiveButton
            { label = text "Cancel"
            , clickMsg = CancelSelection
            , pressed = False
            }
        ]



-- }}}
-- {{{ Overview Plots


overviewPlots : Element msg
overviewPlots =
    column
        [ spacing 10, width fill ]
        [ el [ centerX ] <|
            paragraph []
                [ text "(Click on the bars to see design details.)"
                ]
        , Keyed.el [ centerX ]
            ( "overview"
            , Html.div
                [ HAtt.id "overview"
                , HAtt.style "width" "100%"
                ]
                [ Html.div
                    [ HAtt.style "height" "200px"
                    , HAtt.style "width" "100%"
                    , HAtt.style "border-radius" "5px"
                    , HAtt.style "background-color" "#d3d3d3"
                    ]
                    []
                ]
                |> html
            )
        ]


makeOverViewSpec : Device -> Dict String Design.DesignStub -> { plotId : String, spec : VL.Spec }
makeOverViewSpec device designStubs =
    { plotId = "overview"
    , spec =
        overviewSpec
            device
            (designStubs
                |> Dict.toList
                |> List.filterMap makePlotData
            )
    }


makePlotData : ( String, Design.DesignStub ) -> Maybe PlotData
makePlotData ( uuid, { name, metricsJobStatus } ) =
    WebSockets.getDesignMetrics metricsJobStatus
        |> Maybe.map
            (\metrics ->
                { uuid = uuid
                , name = name
                , hydrophobicFitness = Maybe.withDefault (0 / 0) metrics.hydrophobicFitness
                , isoelectricPoint = metrics.isoelectricPoint
                , numberOfResidues = metrics.numOfResidues |> toFloat
                , packingDensity = metrics.packingDensity
                , budeFFTotalEnergy =
                    Maybe.withDefault (0 / 0)
                        metrics.budeFFResults.totalEnergy
                , evoEFTotalEnergy =
                    Maybe.withDefault (0 / 0)
                        metrics.evoEF2Results.total
                , dfireTotalEnergy =
                    Maybe.withDefault (0 / 0)
                        metrics.dfire2Results.total
                , rosettaTotalEnergy =
                    Maybe.withDefault (0 / 0)
                        metrics.rosettaResults.total_score
                , aggrescan3dTotalValue =
                    Maybe.withDefault (0 / 0)
                        metrics.aggrescan3dResults.total_value
                }
            )


type alias PlotData =
    { uuid : String
    , name : String
    , hydrophobicFitness : Float
    , isoelectricPoint : Float
    , numberOfResidues : Float
    , packingDensity : Float
    , budeFFTotalEnergy : Float
    , evoEFTotalEnergy : Float
    , dfireTotalEnergy : Float
    , rosettaTotalEnergy : Float
    , aggrescan3dTotalValue : Float
    }


plotTuples : List ( String, PlotData -> Float, VL.SortProperty )
plotTuples =
    [ ( "Hydrophobic Fitness", .hydrophobicFitness, VL.soAscending )
    , ( "Isoelectric Point", .isoelectricPoint, VL.soDescending )
    , ( "Number of Residues", .numberOfResidues, VL.soDescending )
    , ( "Mean Packing Density", .packingDensity, VL.soDescending )
    , ( "BUDE FF Total Energy", .budeFFTotalEnergy, VL.soAscending )
    , ( "EvoEF2 Total Energy", .evoEFTotalEnergy, VL.soAscending )
    , ( "dFire2 Total Energy", .dfireTotalEnergy, VL.soAscending )
    , ( "Rosetta Total Energy", .rosettaTotalEnergy, VL.soAscending )
    , ( "Aggrescan3D Total Value", .aggrescan3dTotalValue, VL.soAscending )
    ]


overviewSpec : Device -> List PlotData -> VL.Spec
overviewSpec device plotData =
    let
        metricValueColumn ( label, valueFn, _ ) =
            VL.dataColumn
                label
                (VL.nums <|
                    List.map valueFn plotData
                )

        metricValueBarSpec ( label, _, sortProp ) =
            VL.asSpec
                [ VL.bar []
                , VL.title (label ++ "\nfor All Designs") []
                , VL.width 200
                , (VL.encoding
                    << VL.position VL.Y
                        [ VL.pName "Design Name"
                        , VL.pNominal
                        , VL.pSort
                            [ VL.soByField label VL.opMax
                            , sortProp
                            ]
                        , VL.pAxis
                            [ VL.axLabelExpr
                                "split(datum.label, '@@@')[0]"
                            ]
                        ]
                    << VL.position VL.X
                        [ VL.pName label
                        , VL.pMType VL.Quantitative
                        , VL.pAxis [ VL.axTitle label, VL.axGrid True ]
                        ]
                    << VL.hyperlink
                        [ VL.hName "url"
                        , VL.hNominal
                        ]
                  )
                    []
                ]

        dataColumns =
            List.map metricValueColumn plotTuples
                |> List.foldr (<<) identity

        data =
            VL.dataFromColumns []
                << VL.dataColumn
                    "Design Name"
                    (VL.strs <|
                        List.map (\{ name, uuid } -> name ++ "@@@" ++ uuid) plotData
                    )
                << VL.dataColumn
                    "uuid"
                    (VL.strs <|
                        List.map .uuid plotData
                    )
                << dataColumns

        config =
            (VL.configure
                << VL.configuration (VL.coView [ VL.vicoStroke <| Just "transparent" ])
                << VL.configuration (VL.coAxis [ VL.axcoDomainWidth 1 ])
            )
                []

        transform =
            VL.transform
                << VL.calculateAs "'/de-stress/designs/' + datum.uuid" "url"
                << VL.calculateAs "datum.name + datum.uuid" "unique name"

        ( headTuples, tailTuples ) =
            List.indexedMap Tuple.pair plotTuples
                |> List.partition (\( n, _ ) -> n < (List.length plotTuples // 2))
                |> (\( a, b ) -> ( List.map Tuple.second a, List.map Tuple.second b ))
    in
    VL.toVegaLite
        [ data []
        , VL.spacing 40
        , transform []
        , config
        , case ( device.class, device.orientation ) of
            ( Phone, Portrait ) ->
                VL.vConcat <|
                    List.map metricValueBarSpec plotTuples

            _ ->
                VL.hConcat
                    [ VL.asSpec
                        [ VL.vConcat <|
                            List.map metricValueBarSpec headTuples
                        ]
                    , VL.asSpec
                        [ VL.vConcat <|
                            List.map metricValueBarSpec tailTuples
                        ]
                    ]
        ]



-- }}}
-- }}}
-- {{{ Utils


stringToTags : String -> Set.Set String
stringToTags tagString =
    tagString
        |> String.split ","
        |> List.map String.trim
        |> List.filter (String.isEmpty >> not)
        |> Set.fromList


getAllTags : Dict String Design.StoredDesign -> Set.Set String
getAllTags designDict =
    designDict
        |> Dict.values
        |> List.map (Design.storedDesignToStub >> .tags >> Set.toList)
        |> List.concat
        |> Set.fromList



-- }}}
