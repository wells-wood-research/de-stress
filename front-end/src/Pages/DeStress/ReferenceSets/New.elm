module Pages.DeStress.ReferenceSets.New exposing (Model, Msg, Params, page)

import BigStructure.Object.Aggrescan3DResults as Aggrescan3DResults
import BigStructure.Object.BiolUnit as BiolUnit
import BigStructure.Object.BudeFFResults as BudeFFResults
import BigStructure.Object.DFIRE2Results as DFIRE2Results
import BigStructure.Object.EvoEF2Results as EvoEF2Results
import BigStructure.Object.Pdb as Pdb
import BigStructure.Object.RosettaResults as RosettaResults
import BigStructure.Object.State as State
import BigStructure.Query as Query
import Browser.Navigation as Nav
import Codec
import Dict exposing (Dict)
import Element exposing (..)
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import List.Extra as Le
import RemoteData as Rd exposing (RemoteData)
import Set exposing (Set)
import Shared
import Shared.Buttons as Buttons
import Shared.Design as Design exposing (StoredDesign)
import Shared.Error as Error
import Shared.Metrics as Metrics exposing (RefSetMetrics)
import Shared.ReferenceSet as ReferenceSet exposing (ReferenceSet)
import Shared.ResourceUuid as ResourceUuid exposing (ResourceUuid)
import Shared.Style as Style
import Shared.WebSockets as WebSockets
import Spa.Document exposing (Document)
import Spa.Generated.Route as Route
import Spa.Page as Page exposing (Page)
import Spa.Url exposing (Url)
import Utils.Route exposing (navigate)


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



-- {{{ MODEL


type alias Model =
    { mResourceUuid : Maybe ResourceUuid
    , pageState : PageState
    , designs : Dict String Design.StoredDesign
    , referenceSets : Dict String ReferenceSet.StoredReferenceSet
    , navKey : Nav.Key
    , pageErrors : List Error.Error
    }


type PageState
    = ChoosingRefSetType ConstructionMethod
    | BuildingReferenceSet NewReferenceSet
    | CompletedBuilding
        { uuidString : String
        , failedCodes : Set String
        , referenceSet : ReferenceSet
        }
    | FailedToBuild String


type ConstructionMethod
    = Default DefaultReferenceSet
    | MyDesigns DesignsListModel
    | PdbCodeList PdbCodeListModel


type DefaultReferenceSet
    = Top500
    | Pisces


type alias DesignsListModel =
    { mName : Maybe String
    , mDescription : Maybe String
    , selectedUuids : Set String
    }


type alias PdbCodeListModel =
    { mName : Maybe String
    , mDescription : Maybe String
    , rawPdbCodeListInput : String
    , pdbCodeList : Set String
    , malformedPdbCodes : Set String
    }


codeListModelToNewRefSet : String -> PdbCodeListModel -> NewReferenceSet
codeListModelToNewRefSet uuidString codeListModel =
    { id = uuidString
    , name = Maybe.withDefault "DEFAULT NAME" codeListModel.mName
    , description = Maybe.withDefault "DEFAULT DESCRIPTION" codeListModel.mDescription
    , pdbCodes = codeListModel.pdbCodeList
    , metricsRemoteData = initialMetricsRemoteData
    }


defaultDesignListModel : DesignsListModel
defaultDesignListModel =
    { mName = Nothing
    , mDescription = Nothing
    , selectedUuids = Set.empty
    }


defaultCodeListModel : PdbCodeListModel
defaultCodeListModel =
    { mName = Nothing
    , mDescription = Nothing
    , rawPdbCodeListInput = ""
    , pdbCodeList = Set.empty
    , malformedPdbCodes = Set.empty
    }


type alias NewReferenceSet =
    { id : String
    , name : String
    , description : String
    , pdbCodes : Set String
    , metricsRemoteData : MetricsRemoteData
    }


type alias MetricsRemoteData =
    { basicMetrics : BasicMetricsRemoteData
    , budeFFTotal : BudeFFTotalRemoteData
    , evoEF2Total : EvoEF2TotalRemoteData
    , dfire2Total : Dfire2TotalRemoteData
    , rosettaTotal : RosettaTotalRemoteData
    , aggrescan3DTotal : Aggrescan3DTotalRemoteData
    }


initialMetricsRemoteData : MetricsRemoteData
initialMetricsRemoteData =
    { basicMetrics = Rd.Loading
    , budeFFTotal = Rd.Loading
    , evoEF2Total = Rd.Loading
    , rosettaTotal = Rd.Loading
    , dfire2Total = Rd.Loading
    , aggrescan3DTotal = Rd.Loading
    }


isMetricsDownloadComplete : MetricsRemoteData -> Bool
isMetricsDownloadComplete metricsRemoteData =
    [ Rd.isLoading metricsRemoteData.basicMetrics
    , Rd.isLoading metricsRemoteData.budeFFTotal
    , Rd.isLoading metricsRemoteData.dfire2Total
    , Rd.isLoading metricsRemoteData.evoEF2Total
    , Rd.isLoading metricsRemoteData.rosettaTotal
    , Rd.isLoading metricsRemoteData.aggrescan3DTotal
    ]
        |> List.all not


errToStr : String -> RemoteData e a -> RemoteData String a
errToStr metricName rd =
    Rd.mapError (always <| "failed to download " ++ metricName ++ " metrics") rd


getBuildProgress : NewReferenceSet -> { max : Int, current : Int }
getBuildProgress { metricsRemoteData } =
    let
        rdToInt rd =
            case rd of
                Rd.NotAsked ->
                    0

                Rd.Loading ->
                    0

                Rd.Failure _ ->
                    1

                Rd.Success _ ->
                    1

        progressValues =
            [ rdToInt metricsRemoteData.basicMetrics
            , rdToInt metricsRemoteData.budeFFTotal
            , rdToInt metricsRemoteData.dfire2Total
            , rdToInt metricsRemoteData.evoEF2Total
            , rdToInt metricsRemoteData.rosettaTotal
            , rdToInt metricsRemoteData.aggrescan3DTotal
            ]
    in
    { max = List.length progressValues
    , current = List.sum progressValues
    }


pageStateForCompletedDownload : NewReferenceSet -> MetricsRemoteData -> PageState
pageStateForCompletedDownload newRefSet metricsRemoteData =
    let
        mergedMetricsRemoteData =
            Rd.map
                (convertAllToRefSetMetrics newRefSet.pdbCodes)
                (errToStr "basic" metricsRemoteData.basicMetrics)
                |> Rd.andMap
                    (errToStr "bude ff" metricsRemoteData.budeFFTotal)
                |> Rd.andMap
                    (errToStr "evoef2" metricsRemoteData.evoEF2Total)
                |> Rd.andMap
                    (errToStr "dfire2" metricsRemoteData.dfire2Total)
                |> Rd.andMap
                    (errToStr "rosetta" metricsRemoteData.rosettaTotal)
                |> Rd.andMap
                    (errToStr "aggrescan3d" metricsRemoteData.aggrescan3DTotal)
    in
    case mergedMetricsRemoteData of
        Rd.Success (Ok { failedCodes, metrics }) ->
            CompletedBuilding
                { uuidString = newRefSet.id
                , failedCodes = failedCodes
                , referenceSet =
                    { name = newRefSet.name
                    , description = newRefSet.description
                    , pdbCodeList = Set.empty
                    , metrics = metrics
                    , aggregateData = Metrics.createAggregateData metrics
                    , deleteStatus = Buttons.initDangerStatus
                    }
                }

        Rd.Success (Err e) ->
            FailedToBuild e

        Rd.Failure e ->
            FailedToBuild e

        _ ->
            FailedToBuild
                """Something went wrong when requesting
                metrics. Please try again."""


convertAllToRefSetMetrics :
    Set String
    -> List BasicMetrics
    -> List BudeFFTotal
    -> List EvoEF2Total
    -> List Dfire2Total
    -> List RosettaTotal
    -> List Aggrescan3DTotal
    -> Result String { failedCodes : Set String, metrics : List RefSetMetrics }
convertAllToRefSetMetrics initialPdbCodes bm bff evo df ros agg =
    let
        allPdbCodes =
            [ List.map .pdbCode bm
            , List.map .pdbCode bff
            , List.map .pdbCode evo
            , List.map .pdbCode df
            , List.map .pdbCode ros
            , List.map .pdbCode agg
            ]
                |> List.map Set.fromList

        -- This set should be empty, if it's not, it means that when the requests were
        -- made different records were available in each query
        oddPdbCodes =
            allPdbCodes
                |> List.foldl Set.diff Set.empty

        successfulPdbCodes =
            allPdbCodes
                |> List.foldl Set.union Set.empty

        sBm =
            List.sortBy .pdbCode bm

        sBff =
            List.sortBy .pdbCode bff

        sEvo =
            List.sortBy .pdbCode evo

        sDf =
            List.sortBy .pdbCode df

        sRos =
            List.sortBy .pdbCode ros

        sAgg =
            List.sortBy .pdbCode agg
    in
    if Set.isEmpty oddPdbCodes then
        Ok
            { failedCodes = Set.diff initialPdbCodes successfulPdbCodes
            , metrics =
                List.map downloadedToRefSetMetrics sBm
                    |> Le.andMap sBff
                    |> Le.andMap sEvo
                    |> Le.andMap sDf
                    |> Le.andMap sRos
                    |> Le.andMap sAgg
            }

    else
        """Something went wrong when requesting the metrics. I encountered an odd
            situation where a partial set of metrics was available for the following
            entries: """
            ++ String.join ", " (Set.toList oddPdbCodes)
            |> Err


downloadedToRefSetMetrics :
    BasicMetrics
    -> BudeFFTotal
    -> EvoEF2Total
    -> Dfire2Total
    -> RosettaTotal
    -> Aggrescan3DTotal
    -> RefSetMetrics
downloadedToRefSetMetrics bm bff evo df ros agg =
    { pdbCode = bm.pdbCode
    , composition = bm.composition
    , torsionAngles = bm.torsionAngles
    , hydrophobicFitness = bm.hydrophobicFitness
    , isoelectricPoint = bm.isoelectricPoint
    , mass = bm.mass
    , numberOfResidues = bm.numberOfResidues
    , packingDensity = bm.packingDensity
    , budeFFTotalEnergy = bff.budeFFTotal
    , evoEFTotalEnergy = evo.evoEF2Total
    , dfireTotalEnergy = df.dfire2Total
    , rosettaTotalEnergy = ros.rosettaTotal
    , aggrescan3dTotalValue = agg.aggrescan3DTotal
    }


constructionMethodToString : ConstructionMethod -> String
constructionMethodToString constructionMethod =
    case constructionMethod of
        Default refSet ->
            case refSet of
                Top500 ->
                    top500.name

                Pisces ->
                    pisces.name

        MyDesigns _ ->
            "My Designs"

        PdbCodeList _ ->
            "PDB List"



-- }}}
-- {{{ INIT


type alias Params =
    ()


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared _ =
    case Shared.getRunState shared of
        Just runState ->
            ( { mResourceUuid = Just runState.resourceUuid
              , pageState = ChoosingRefSetType <| Default Top500
              , designs = runState.designs
              , referenceSets = runState.referenceSets
              , navKey = shared.key
              , pageErrors = []
              }
            , Cmd.none
            )

        Nothing ->
            ( { mResourceUuid = Nothing
              , pageState = ChoosingRefSetType <| Default Top500
              , designs = Dict.empty
              , referenceSets = Dict.empty
              , navKey = shared.key
              , pageErrors = []
              }
            , Cmd.none
            )



-- }}}
-- {{{ UPDATE


type Msg
    = UpdatedConstructionMethod ConstructionMethod
    | UpdatedName String
    | UpdatedDescription String
    | SelectDesign String Bool
    | UpdatedPdbCodes String
    | ClickedDownloadRefSet
    | GotBasicMetrics BasicMetricsRemoteData
    | GotBudeFFMetrics BudeFFTotalRemoteData
    | GotEvoEF2Metrics EvoEF2TotalRemoteData
    | GotDfire2Metrics Dfire2TotalRemoteData
    | GotRosettaMetrics RosettaTotalRemoteData
    | GotAggrescan3DMetrics Aggrescan3DTotalRemoteData
    | ClickedCreateReferenceSet
    | ClickedCancelCreate
    | ClearPageErrors


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdatedConstructionMethod newConstructionMethod ->
            ( { model | pageState = ChoosingRefSetType newConstructionMethod }
            , Cmd.none
            )

        UpdatedName name ->
            case model.pageState of
                ChoosingRefSetType (PdbCodeList codeListModel) ->
                    if String.isEmpty name then
                        ( { model
                            | pageState =
                                ChoosingRefSetType <|
                                    PdbCodeList
                                        { codeListModel | mName = Nothing }
                          }
                        , Cmd.none
                        )

                    else
                        ( { model
                            | pageState =
                                ChoosingRefSetType <|
                                    PdbCodeList
                                        { codeListModel | mName = Just name }
                          }
                        , Cmd.none
                        )

                ChoosingRefSetType (MyDesigns designsListModel) ->
                    if String.isEmpty name then
                        ( { model
                            | pageState =
                                ChoosingRefSetType <|
                                    MyDesigns
                                        { designsListModel | mName = Nothing }
                          }
                        , Cmd.none
                        )

                    else
                        ( { model
                            | pageState =
                                ChoosingRefSetType <|
                                    MyDesigns
                                        { designsListModel | mName = Just name }
                          }
                        , Cmd.none
                        )

                _ ->
                    updateWithUnexpectedStateError model

        UpdatedDescription description ->
            case model.pageState of
                ChoosingRefSetType (PdbCodeList codeListModel) ->
                    if String.isEmpty description then
                        ( { model
                            | pageState =
                                ChoosingRefSetType <|
                                    PdbCodeList
                                        { codeListModel | mDescription = Nothing }
                          }
                        , Cmd.none
                        )

                    else
                        ( { model
                            | pageState =
                                ChoosingRefSetType <|
                                    PdbCodeList
                                        { codeListModel | mDescription = Just description }
                          }
                        , Cmd.none
                        )

                ChoosingRefSetType (MyDesigns designsListModel) ->
                    if String.isEmpty description then
                        ( { model
                            | pageState =
                                ChoosingRefSetType <|
                                    MyDesigns
                                        { designsListModel | mDescription = Nothing }
                          }
                        , Cmd.none
                        )

                    else
                        ( { model
                            | pageState =
                                ChoosingRefSetType <|
                                    MyDesigns
                                        { designsListModel | mDescription = Just description }
                          }
                        , Cmd.none
                        )

                _ ->
                    updateWithUnexpectedStateError model

        SelectDesign uuidString checked ->
            case model.pageState of
                ChoosingRefSetType (MyDesigns designsListModel) ->
                    ( { model
                        | pageState =
                            ChoosingRefSetType <|
                                MyDesigns
                                    { designsListModel
                                        | selectedUuids =
                                            if checked then
                                                Set.insert
                                                    uuidString
                                                    designsListModel.selectedUuids

                                            else
                                                Set.remove
                                                    uuidString
                                                    designsListModel.selectedUuids
                                    }
                      }
                    , Cmd.none
                    )

                _ ->
                    updateWithUnexpectedStateError model

        UpdatedPdbCodes rawCodes ->
            case model.pageState of
                ChoosingRefSetType (PdbCodeList codeListModel) ->
                    let
                        isPdbCode str =
                            (String.length str == 4)
                                && (String.toList str
                                        |> List.all Char.isAlphaNum
                                   )

                        ( good, malformed ) =
                            String.words rawCodes
                                |> Set.fromList
                                |> Set.partition isPdbCode
                    in
                    ( { model
                        | pageState =
                            ChoosingRefSetType <|
                                PdbCodeList
                                    { codeListModel
                                        | rawPdbCodeListInput = rawCodes
                                        , pdbCodeList = good
                                        , malformedPdbCodes = malformed
                                    }
                      }
                    , Cmd.none
                    )

                _ ->
                    updateWithUnexpectedStateError model

        ClickedDownloadRefSet ->
            case ( model.pageState, model.mResourceUuid ) of
                ( ChoosingRefSetType refSetType, Just resourceUuid ) ->
                    let
                        { uuidString, nextResourceUuid } =
                            ResourceUuid.toString resourceUuid

                        newRefSet =
                            case refSetType of
                                Default Top500 ->
                                    top500

                                Default Pisces ->
                                    pisces

                                MyDesigns _ ->
                                    Debug.todo "Add this"

                                PdbCodeList codeListModel ->
                                    codeListModelToNewRefSet uuidString codeListModel
                    in
                    ( { model
                        | mResourceUuid = Just nextResourceUuid
                        , pageState =
                            BuildingReferenceSet
                                newRefSet
                      }
                    , Cmd.batch
                        [ basicMetricsQuery newRefSet.pdbCodes
                            |> Graphql.Http.queryRequest
                                "https://pragmaticproteindesign.bio.ed.ac.uk/big-structure/graphql"
                            |> Graphql.Http.send
                                (Rd.fromResult >> GotBasicMetrics)
                        , budeFFTotalQuery newRefSet.pdbCodes
                            |> Graphql.Http.queryRequest
                                "https://pragmaticproteindesign.bio.ed.ac.uk/big-structure/graphql"
                            |> Graphql.Http.send
                                (Rd.fromResult >> GotBudeFFMetrics)
                        , evoEF2TotalQuery newRefSet.pdbCodes
                            |> Graphql.Http.queryRequest
                                "https://pragmaticproteindesign.bio.ed.ac.uk/big-structure/graphql"
                            |> Graphql.Http.send
                                (Rd.fromResult >> GotEvoEF2Metrics)
                        , dfire2TotalQuery newRefSet.pdbCodes
                            |> Graphql.Http.queryRequest
                                "https://pragmaticproteindesign.bio.ed.ac.uk/big-structure/graphql"
                            |> Graphql.Http.send
                                (Rd.fromResult >> GotDfire2Metrics)
                        , rosettaTotalQuery newRefSet.pdbCodes
                            |> Graphql.Http.queryRequest
                                "https://pragmaticproteindesign.bio.ed.ac.uk/big-structure/graphql"
                            |> Graphql.Http.send
                                (Rd.fromResult >> GotRosettaMetrics)
                        , aggrescan3DTotalQuery newRefSet.pdbCodes
                            |> Graphql.Http.queryRequest
                                "https://pragmaticproteindesign.bio.ed.ac.uk/big-structure/graphql"
                            |> Graphql.Http.send
                                (Rd.fromResult >> GotAggrescan3DMetrics)
                        ]
                    )

                _ ->
                    updateWithUnexpectedStateError model

        GotBasicMetrics remoteData ->
            case model.pageState of
                BuildingReferenceSet ({ metricsRemoteData } as newRefSet) ->
                    let
                        updatedMetricsRemoteData =
                            { metricsRemoteData
                                | basicMetrics = remoteData
                            }
                    in
                    ( { model
                        | pageState =
                            if isMetricsDownloadComplete updatedMetricsRemoteData then
                                pageStateForCompletedDownload
                                    newRefSet
                                    updatedMetricsRemoteData

                            else
                                BuildingReferenceSet
                                    { newRefSet
                                        | metricsRemoteData =
                                            updatedMetricsRemoteData
                                    }
                      }
                    , Cmd.none
                    )

                _ ->
                    updateWithUnexpectedStateError model

        GotBudeFFMetrics remoteData ->
            case model.pageState of
                BuildingReferenceSet ({ metricsRemoteData } as newRefSet) ->
                    let
                        updatedMetricsRemoteData =
                            { metricsRemoteData
                                | budeFFTotal = remoteData
                            }
                    in
                    ( { model
                        | pageState =
                            if isMetricsDownloadComplete updatedMetricsRemoteData then
                                pageStateForCompletedDownload
                                    newRefSet
                                    updatedMetricsRemoteData

                            else
                                BuildingReferenceSet
                                    { newRefSet
                                        | metricsRemoteData =
                                            updatedMetricsRemoteData
                                    }
                      }
                    , Cmd.none
                    )

                _ ->
                    updateWithUnexpectedStateError model

        GotEvoEF2Metrics remoteData ->
            case model.pageState of
                BuildingReferenceSet ({ metricsRemoteData } as newRefSet) ->
                    let
                        updatedMetricsRemoteData =
                            { metricsRemoteData
                                | evoEF2Total = remoteData
                            }
                    in
                    ( { model
                        | pageState =
                            if isMetricsDownloadComplete updatedMetricsRemoteData then
                                pageStateForCompletedDownload
                                    newRefSet
                                    updatedMetricsRemoteData

                            else
                                BuildingReferenceSet
                                    { newRefSet
                                        | metricsRemoteData =
                                            updatedMetricsRemoteData
                                    }
                      }
                    , Cmd.none
                    )

                _ ->
                    updateWithUnexpectedStateError model

        GotDfire2Metrics remoteData ->
            case model.pageState of
                BuildingReferenceSet ({ metricsRemoteData } as newRefSet) ->
                    let
                        updatedMetricsRemoteData =
                            { metricsRemoteData
                                | dfire2Total = remoteData
                            }
                    in
                    ( { model
                        | pageState =
                            if isMetricsDownloadComplete updatedMetricsRemoteData then
                                pageStateForCompletedDownload
                                    newRefSet
                                    updatedMetricsRemoteData

                            else
                                BuildingReferenceSet
                                    { newRefSet
                                        | metricsRemoteData =
                                            updatedMetricsRemoteData
                                    }
                      }
                    , Cmd.none
                    )

                _ ->
                    updateWithUnexpectedStateError model

        GotRosettaMetrics remoteData ->
            case model.pageState of
                BuildingReferenceSet ({ metricsRemoteData } as newRefSet) ->
                    let
                        updatedMetricsRemoteData =
                            { metricsRemoteData
                                | rosettaTotal = remoteData
                            }
                    in
                    ( { model
                        | pageState =
                            if isMetricsDownloadComplete updatedMetricsRemoteData then
                                pageStateForCompletedDownload
                                    newRefSet
                                    updatedMetricsRemoteData

                            else
                                BuildingReferenceSet
                                    { newRefSet
                                        | metricsRemoteData =
                                            updatedMetricsRemoteData
                                    }
                      }
                    , Cmd.none
                    )

                _ ->
                    updateWithUnexpectedStateError model

        GotAggrescan3DMetrics remoteData ->
            case model.pageState of
                BuildingReferenceSet ({ metricsRemoteData } as newRefSet) ->
                    let
                        updatedMetricsRemoteData =
                            { metricsRemoteData
                                | aggrescan3DTotal = remoteData
                            }
                    in
                    ( { model
                        | pageState =
                            if isMetricsDownloadComplete updatedMetricsRemoteData then
                                pageStateForCompletedDownload
                                    newRefSet
                                    updatedMetricsRemoteData

                            else
                                BuildingReferenceSet
                                    { newRefSet
                                        | metricsRemoteData =
                                            updatedMetricsRemoteData
                                    }
                      }
                    , Cmd.none
                    )

                _ ->
                    updateWithUnexpectedStateError model

        ClickedCreateReferenceSet ->
            case ( model.pageState, model.mResourceUuid ) of
                ( ChoosingRefSetType (MyDesigns designsListModel), Just resourceUuid ) ->
                    let
                        { uuidString, nextResourceUuid } =
                            ResourceUuid.toString resourceUuid

                        selectedDesigns =
                            model.designs
                                |> Dict.toList
                                |> List.filter
                                    (\( uuid, _ ) ->
                                        Set.member uuid
                                            designsListModel.selectedUuids
                                    )
                                |> List.map Tuple.second
                                |> List.map Design.storedDesignToStub

                        designMetrics =
                            selectedDesigns
                                |> List.filterMap
                                    (.metricsJobStatus >> WebSockets.getDesignMetrics)

                        refSetMetrics =
                            List.map2
                                Metrics.refSetMetricsFromDesignMetrics
                                (List.map .fileName selectedDesigns)
                                designMetrics

                        newReferenceSet =
                            { name =
                                designsListModel.mName
                                    |> Maybe.withDefault "DEFAULT NAME"
                            , description =
                                designsListModel.mDescription
                                    |> Maybe.withDefault "DEFAULT DESCRIPTION"
                            , pdbCodeList =
                                Set.fromList <|
                                    List.map .fileName selectedDesigns
                            , metrics = refSetMetrics
                            , aggregateData =
                                refSetMetrics
                                    |> Metrics.createAggregateData
                            , deleteStatus = Buttons.initDangerStatus
                            }
                    in
                    ( { model
                        | mResourceUuid = Just nextResourceUuid
                        , pageState = ChoosingRefSetType <| Default Top500
                        , referenceSets =
                            Dict.insert
                                uuidString
                                (newReferenceSet
                                    |> ReferenceSet.createReferenceSetStub
                                    |> ReferenceSet.storeReferenceSetStubLocally
                                )
                                model.referenceSets
                      }
                    , Cmd.batch
                        [ ReferenceSet.storeReferenceSet
                            { uuidString = uuidString
                            , referenceSet =
                                Codec.encoder
                                    ReferenceSet.codec
                                    newReferenceSet
                            }
                        , navigate model.navKey Route.ReferenceSets
                        ]
                    )

                ( CompletedBuilding { uuidString, referenceSet }, _ ) ->
                    ( { model
                        | pageState = ChoosingRefSetType <| Default Top500
                        , referenceSets =
                            Dict.insert
                                uuidString
                                (referenceSet
                                    |> ReferenceSet.createReferenceSetStub
                                    |> ReferenceSet.storeReferenceSetStubLocally
                                )
                                model.referenceSets
                      }
                    , Cmd.batch
                        [ ReferenceSet.storeReferenceSet
                            { uuidString = uuidString
                            , referenceSet =
                                Codec.encoder
                                    ReferenceSet.codec
                                    referenceSet
                            }
                        , navigate model.navKey Route.DeStress__ReferenceSets
                        ]
                    )

                _ ->
                    updateWithUnexpectedStateError model

        ClickedCancelCreate ->
            ( model
            , navigate model.navKey Route.DeStress__ReferenceSets
            )

        ClearPageErrors ->
            ( { model | pageErrors = [] }
            , Cmd.none
            )


updateWithUnexpectedStateError : Model -> ( Model, Cmd Msg )
updateWithUnexpectedStateError model =
    Error.updateWithError
        ClearPageErrors
        { model | pageState = ChoosingRefSetType (Default Top500) }
        { title = "Failed to create reference set"
        , details =
            """ Looks like there was an error when creating your reference set. Try
            refreshing your browser and if this problem persists, report it as a bug.
            See the home page for details on how to do this.
            """
        , severity = Error.Medium
        }


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
                        , referenceSets = model.referenceSets
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
            ( { model
                | mResourceUuid = Just runState.resourceUuid
                , referenceSets = runState.referenceSets
              }
            , Cmd.none
            )

        Nothing ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- }}}
-- {{{ VIEW


view : Model -> Document Msg
view model =
    { title = "Create New Reference Set"
    , body = [ bodyView model ]
    }


bodyView : Model -> Element Msg
bodyView model =
    column
        [ Style.pageWidths.singleColumn
        , centerX
        , spacing 30
        ]
        [ text "Create New Reference Set"
            |> Style.h1
            |> el [ centerX ]
        , case model.pageState of
            ChoosingRefSetType currentChoice ->
                column [ width fill, spacing 15 ]
                    [ row Style.scrollOptions <|
                        (List.map
                            (refSetTypeSelector currentChoice)
                            [ Default Top500
                            , Default Pisces
                            , MyDesigns defaultDesignListModel
                            , PdbCodeList defaultCodeListModel
                            ]
                            |> List.intersperse (text "|")
                        )
                    , case currentChoice of
                        Default refSet ->
                            defaultRefSetView refSet

                        MyDesigns designsListModel ->
                            designsListView model.designs designsListModel

                        PdbCodeList codeListModel ->
                            pdbCodeListView codeListModel
                    ]

            BuildingReferenceSet newRefSet ->
                buildingProgressView newRefSet

            CompletedBuilding { failedCodes, referenceSet } ->
                completedBuildingView failedCodes referenceSet

            FailedToBuild error ->
                paragraph
                    []
                    [ text error ]
        ]


refSetTypeSelector : ConstructionMethod -> ConstructionMethod -> Element Msg
refSetTypeSelector current option =
    constructionMethodToString option
        |> text
        |> el
            ((if constructionMethodToString current == constructionMethodToString option then
                [ Font.underline ]

              else
                [ Events.onClick <| UpdatedConstructionMethod option ]
             )
                ++ [ pointer ]
            )


defaultRefSetView : DefaultReferenceSet -> Element Msg
defaultRefSetView defaultSet =
    let
        refSet =
            case defaultSet of
                Top500 ->
                    top500

                Pisces ->
                    pisces
    in
    column [ width fill, spacing 30 ]
        [ paragraph []
            [ text refSet.description
            ]
        , Buttons.alwaysActiveButton
            { clickMsg =
                ClickedDownloadRefSet
            , label = text "Download Reference Set"
            , pressed = False
            }
        ]


buildingProgressView : NewReferenceSet -> Element msg
buildingProgressView newRefSet =
    column
        [ spacing 10, width fill ]
        [ paragraph
            []
            [ text "Downloading reference set..." ]
        , getBuildProgress newRefSet
            |> Style.progressBar
        ]


completedBuildingView : Set String -> ReferenceSet -> Element Msg
completedBuildingView failedCodes refSet =
    column
        [ spacing 15, width fill ]
        ([ paragraph
            []
            [ text "Finished downloading data." ]
         , Style.progressBar { max = 5, current = 5 }
         , paragraph
            []
            [ text "Metrics ran successfully for "
            , refSet.metrics
                |> List.length
                |> String.fromInt
                |> text
            , text " structures. Would you like to create the reference set?"
            ]
         , wrappedRow [ spacing 10 ]
            [ Buttons.alwaysActiveButton
                { label = text "Create"
                , clickMsg = ClickedCreateReferenceSet
                , pressed = False
                }
            , Buttons.alwaysActiveButton
                { label = text "Cancel"
                , clickMsg = ClickedCancelCreate
                , pressed = False
                }
            ]
         ]
            ++ (if not <| Set.isEmpty failedCodes then
                    [ Style.h2 <| text "Warning"
                    , paragraph
                        []
                        [ text
                            """Big Structure does not contain metrics for all
                            structures, as it is either impractical or impossible to run
                            the metrics for certain files. The following files are
                            currently unavailable:
                            """
                        ]
                    , paragraph
                        []
                        [ Set.toList failedCodes
                            |> String.join " "
                            |> text
                        ]
                    ]

                else
                    []
               )
        )


designsListView : Dict String StoredDesign -> DesignsListModel -> Element Msg
designsListView designs designsListModel =
    column [ width fill, spacing 15 ]
        [ paragraph []
            [ text
                """Create a reference set from your own uploaded designs."""
            ]
        , Input.text
            Style.textInputStyle
            { onChange = UpdatedName
            , text = Maybe.withDefault "" designsListModel.mName
            , placeholder = Nothing
            , label =
                Input.labelAbove []
                    (Style.h2 <| text "Name")
            }
        , Input.multiline
            Style.textInputStyle
            { onChange = UpdatedDescription
            , text = Maybe.withDefault "" designsListModel.mDescription
            , placeholder = Nothing
            , label =
                Input.labelAbove []
                    (Style.h2 <| text "Description")
            , spellcheck = True
            }
        , Style.h2 <| text "Designs"
        , text "Select designs to include in the reference set."
        , Dict.toList designs
            |> List.map (designSelectView designsListModel.selectedUuids)
            |> column []
        , let
            validName =
                case designsListModel.mName of
                    Nothing ->
                        False

                    Just _ ->
                        True

            validDescription =
                case designsListModel.mDescription of
                    Nothing ->
                        False

                    Just _ ->
                        True

            complete =
                validName && validDescription && (Set.isEmpty >> not) designsListModel.selectedUuids
          in
          Buttons.conditionalButton
            { clickMsg =
                Just ClickedCreateReferenceSet
            , label = text "Create Reference Set"
            , isActive = complete
            }
        ]


designSelectView : Set String -> ( String, StoredDesign ) -> Element Msg
designSelectView selectedUuids ( uuidString, storedDesign ) =
    let
        designStub =
            Design.storedDesignToStub storedDesign
    in
    row [ spacing 10 ]
        [ Input.checkbox []
            { onChange = SelectDesign uuidString
            , icon = Input.defaultCheckbox
            , checked = Set.member uuidString selectedUuids
            , label = Input.labelHidden "Design Checkbox"
            }
        , text designStub.name
        ]


pdbCodeListView : PdbCodeListModel -> Element Msg
pdbCodeListView codeListModel =
    column [ width fill, spacing 15 ]
        [ paragraph []
            [ text
                """Create a reference set from a list of PDB codes. The biological unit
                of the structure, as defined on PDBe as assembly 1, will be used to
                create the reference set.
                """
            ]
        , Input.text
            Style.textInputStyle
            { onChange = UpdatedName
            , text = Maybe.withDefault "" codeListModel.mName
            , placeholder = Nothing
            , label =
                Input.labelAbove []
                    (Style.h2 <| text "Name")
            }
        , Input.multiline
            Style.textInputStyle
            { onChange = UpdatedDescription
            , text = Maybe.withDefault "" codeListModel.mDescription
            , placeholder = Nothing
            , label =
                Input.labelAbove []
                    (Style.h2 <| text "Description")
            , spellcheck = True
            }
        , Input.multiline
            (Style.textInputStyle
                ++ [ height <| px 150 ]
            )
            { onChange = UpdatedPdbCodes
            , text = codeListModel.rawPdbCodeListInput
            , placeholder = Nothing
            , label =
                Input.labelAbove []
                    (Style.h2 <|
                        text "PDB Codes (whitespace separated)"
                    )
            , spellcheck = True
            }
        , paragraph []
            [ "Found "
                ++ (Set.size codeListModel.pdbCodeList |> String.fromInt)
                ++ " PDB codes. "
                ++ (Set.size codeListModel.malformedPdbCodes |> String.fromInt)
                ++ " "
                ++ (if Set.size codeListModel.malformedPdbCodes > 1 then
                        "entries do"

                    else
                        "entry does"
                   )
                ++ " not look a like PDB code:\n"
                ++ (Set.toList codeListModel.malformedPdbCodes |> String.join " ")
                |> text
            ]
        , let
            validName =
                case codeListModel.mName of
                    Nothing ->
                        False

                    Just _ ->
                        True

            validDescription =
                case codeListModel.mDescription of
                    Nothing ->
                        False

                    Just _ ->
                        True

            complete =
                validName && validDescription && (Set.isEmpty >> not) codeListModel.pdbCodeList
          in
          Buttons.conditionalButton
            { clickMsg =
                Just ClickedDownloadRefSet
            , label = text "Download Reference Set"
            , isActive = complete
            }
        ]



-- }}}
-- {{{ New Reference Set Queries


type alias BasicMetrics =
    { pdbCode : String
    , composition : Dict String Float
    , torsionAngles : Dict String ( Float, Float, Float )
    , hydrophobicFitness : Maybe Float
    , isoelectricPoint : Float
    , mass : Float
    , numberOfResidues : Int
    , packingDensity : Float
    }


type alias BasicMetricsRemoteData =
    RemoteData (Graphql.Http.Error (List BasicMetrics)) (List BasicMetrics)


basicMetricsQuery : Set String -> SelectionSet (List BasicMetrics) RootQuery
basicMetricsQuery pdbCodeList =
    Query.preferredStatesSubset
        (\optionals -> { optionals | stateNumber = Absent })
        { codes =
            Set.toList pdbCodeList
                |> List.map String.toLower
        }
        (SelectionSet.succeed BasicMetrics
            |> with
                (State.biolUnit (BiolUnit.pdb Pdb.pdbCode)
                    |> SelectionSet.map
                        (\mmPdbCode ->
                            case mmPdbCode of
                                Just (Just pdbCode) ->
                                    pdbCode

                                _ ->
                                    "Unknown PDB"
                        )
                )
            |> with (SelectionSet.map Metrics.compositionStringToDict State.composition)
            |> with (SelectionSet.map Metrics.torsionAngleStringToDict State.torsionAngles)
            |> with State.hydrophobicFitness
            |> with State.isoelectricPoint
            |> with State.mass
            |> with State.numOfResidues
            |> with State.meanPackingDensity
        )


unwrapMMSS : SelectionSet (Maybe (Maybe (Maybe b))) scope -> SelectionSet (Maybe b) scope
unwrapMMSS =
    SelectionSet.map
        (Maybe.andThen (Maybe.andThen identity))


type alias BudeFFTotal =
    { pdbCode : String
    , budeFFTotal : Maybe Float
    }


type alias BudeFFTotalRemoteData =
    RemoteData (Graphql.Http.Error (List BudeFFTotal)) (List BudeFFTotal)


budeFFTotalQuery : Set String -> SelectionSet (List BudeFFTotal) RootQuery
budeFFTotalQuery pdbCodeList =
    Query.preferredBudeSubset
        (\optionals -> { optionals | stateNumber = Absent })
        { codes =
            Set.toList pdbCodeList
                |> List.map String.toLower
        }
        (SelectionSet.succeed BudeFFTotal
            |> with
                (SelectionSet.map (Maybe.withDefault "Unknown PDB")
                    (unwrapMMSS
                        (BudeFFResults.state
                            (State.biolUnit
                                (BiolUnit.pdb
                                    Pdb.pdbCode
                                )
                            )
                        )
                    )
                )
            |> with BudeFFResults.totalEnergy
        )


type alias EvoEF2Total =
    { pdbCode : String
    , evoEF2Total : Maybe Float
    }


type alias EvoEF2TotalRemoteData =
    RemoteData (Graphql.Http.Error (List EvoEF2Total)) (List EvoEF2Total)


evoEF2TotalQuery : Set String -> SelectionSet (List EvoEF2Total) RootQuery
evoEF2TotalQuery pdbCodeList =
    Query.preferredEvoef2Subset
        (\optionals -> { optionals | stateNumber = Absent })
        { codes =
            Set.toList pdbCodeList
                |> List.map String.toLower
        }
        (SelectionSet.succeed EvoEF2Total
            |> with
                (SelectionSet.map (Maybe.withDefault "Unknown PDB")
                    (unwrapMMSS
                        (EvoEF2Results.state
                            (State.biolUnit
                                (BiolUnit.pdb
                                    Pdb.pdbCode
                                )
                            )
                        )
                    )
                )
            |> with EvoEF2Results.total
        )


type alias Dfire2Total =
    { pdbCode : String
    , dfire2Total : Maybe Float
    }


type alias Dfire2TotalRemoteData =
    RemoteData (Graphql.Http.Error (List Dfire2Total)) (List Dfire2Total)


dfire2TotalQuery : Set String -> SelectionSet (List Dfire2Total) RootQuery
dfire2TotalQuery pdbCodeList =
    Query.preferredDfire2Subset
        (\optionals -> { optionals | stateNumber = Absent })
        { codes =
            Set.toList pdbCodeList
                |> List.map String.toLower
        }
        (SelectionSet.succeed Dfire2Total
            |> with
                (SelectionSet.map (Maybe.withDefault "Unknown PDB")
                    (unwrapMMSS
                        (DFIRE2Results.state
                            (State.biolUnit
                                (BiolUnit.pdb
                                    Pdb.pdbCode
                                )
                            )
                        )
                    )
                )
            |> with DFIRE2Results.total
        )


type alias RosettaTotal =
    { pdbCode : String
    , rosettaTotal : Maybe Float
    }


type alias RosettaTotalRemoteData =
    RemoteData (Graphql.Http.Error (List RosettaTotal)) (List RosettaTotal)


rosettaTotalQuery : Set String -> SelectionSet (List RosettaTotal) RootQuery
rosettaTotalQuery pdbCodeList =
    Query.preferredRosettaSubset
        (\optionals -> { optionals | stateNumber = Absent })
        { codes =
            Set.toList pdbCodeList
                |> List.map String.toLower
        }
        (SelectionSet.succeed RosettaTotal
            |> with
                (SelectionSet.map (Maybe.withDefault "Unknown PDB")
                    (unwrapMMSS
                        (RosettaResults.state
                            (State.biolUnit
                                (BiolUnit.pdb
                                    Pdb.pdbCode
                                )
                            )
                        )
                    )
                )
            |> with RosettaResults.totalScore
        )


type alias Aggrescan3DTotal =
    { pdbCode : String
    , aggrescan3DTotal : Maybe Float
    }


type alias Aggrescan3DTotalRemoteData =
    RemoteData (Graphql.Http.Error (List Aggrescan3DTotal)) (List Aggrescan3DTotal)


aggrescan3DTotalQuery : Set String -> SelectionSet (List Aggrescan3DTotal) RootQuery
aggrescan3DTotalQuery pdbCodeList =
    Query.preferredAggrescan3dSubset
        (\optionals -> { optionals | stateNumber = Absent })
        { codes =
            Set.toList pdbCodeList
                |> List.map String.toLower
        }
        (SelectionSet.succeed Aggrescan3DTotal
            |> with
                (SelectionSet.map (Maybe.withDefault "Unknown PDB")
                    (unwrapMMSS
                        (Aggrescan3DResults.state
                            (State.biolUnit
                                (BiolUnit.pdb
                                    Pdb.pdbCode
                                )
                            )
                        )
                    )
                )
            |> with Aggrescan3DResults.totalValue
        )



-- }}}
-- {{{ Default Reference Sets


top500 : NewReferenceSet
top500 =
    { id = "top-500-subset"
    , name = "Top 500"
    , description =
        """A set of high-quality structures defined by the Richardson lab. This is a
        subset of the structures in top500, as the metrics fail to run for some of the
        structure files in the set. Uses the preferred biological unit as defined by
        PDBe.
        """
    , pdbCodes =
        """
        119l 153l 16pk 1a28 1a2p 1a62 1a6m 1a73 1a7s 1a8d 1a8e 1a92 1aay 1aba 1ads 1agj
        1ah7 1aho 1aie 1ajj 1ak0 1ako 1akr 1amf 1amm 1amp 1aoh 1aop 1aqb 1aqz 1arb 1aru
        1atg 1atl 1atz 1auo 1axn 1ay7 1ayl 1b0y 1b16 1b3a 1b5e 1b67 1b6a 1b6g 1b8o 1b9w
        1bb1 1bbh 1bbz 1bdo 1beh 1bf4 1bfd 1bfg 1bg2 1bg6 1bgc 1bgf 1bhp 1bi5 1bj7 1bk0
        1bk7 1bkb 1bkj 1bkr 1bm8 1bpi 1bqc 1bqk 1brt 1bs9 1bsm 1btk 1bx4 1bx7 1bxo 1byi
        1byq 1c02 1c1k 1c1l 1c24 1c3d 1c3p 1c3w 1c52 1c5e 1c75 1c90 1cb0 1cc8 1ccz 1cem
        1cex 1cgo 1chd 1cip 1cjw 1cke 1cl8 1cmb 1cnv 1ctf 1ctj 1cv8 1cvl 1cxc 1cxq 1cxy
        1cyo 1czp 1d2n 1d7p 1dbg 1dcs 1df4 1dfu 1dhn 1di6 1dif 1dnl 1doz 1dp7 1dpt 1dvj
        1dxg 1eco 1edg 1egw 1ek0 1elk 1erv 1erx 1es5 1etn 1euw 1evh 1ezm 1fas 1fdr 1fds
        1fkj 1flm 1flp 1flt 1fmb 1fna 1fnc 1fus 1fvk 1fxd 1g3p 1gai 1gca 1gci 1gdj 1gso
        1gvp 1hcl 1hcr 1hfc 1hka 1hmt 1hpm 1htr 1iab 1ido 1ifc 1iib 1isu 1ixh 1jer 1jet
        1jhg 1kap 1koe 1kp6 1kpf 1kpt 1kuh 1kve 1lam 1lbu 1lcl 1lkk 1lmb 1lst 1m6p 1mba
        1mfi 1mfm 1mgt 1mjh 1mla 1mml 1mof 1mol 1moq 1msi 1msk 1mug 1mun 1nar 1nbc 1ndd
        1nfn 1nif 1nkd 1nkr 1nls 1not 1nox 1npk 1nul 1nwp 1oaa 1onc 1opd 1osa 1pda 1pdo
        1pef 1pen 1pgs 1phn 1plc 1pmi 1poa 1psr 1ptf 1pym 1qau 1qb7 1qcx 1qdd 1qe3 1qf9
        1qgi 1qgq 1qgw 1qh5 1qhf 1qhv 1qj4 1qk5 1ql0 1qnf 1qq4 1qq5 1qqq 1qre 1qrr 1qsa
        1qts 1qtw 1qu9 1qup 1qus 1ra9 1rb9 1rcf 1rge 1rhs 1rie 1rzl 1sbp 1smd 1sml 1stn
        1svf 1svy 1swu 1t1d 1tc1 1tca 1tfe 1tgx 1thv 1tif 1tml 1toa 1tph 1ttb 1tud 1tx4
        1tyv 1uae 1uch 1uro 1ush 1ute 1uxy 1vca 1vcc 1vfr 1vfy 1vhh 1vie 1vjs 1vsr 1wab
        1whi 1xjo 1xnb 1yac 1ytb 1zin 256b 2a0b 2act 2acy 2arc 2ayh 2baa 2bc2 2bop 2cba
        2cbp 2cpg 2cpl 2cpp 2ctc 2cua 2cyp 2dpm 2dri 2end 2erl 2fdn 2gar 2hbg 2hft 2igd
        2ilk 2knt 2lis 2mcm 2mhr 2por 2pth 2pvb 2rn2 2sak 2sn3 2spc 2tgi 2tnf 2trx 3chb
        3chy 3cla 3cyr 3ebx 3eip 3ezm 3hts 3nul 3pte 3pvi 3pyp 3seb 3vub 451c 4eug 4lzt
        5cyt 5hpg 5nul 5p21 6cel 6gsv 7a3h 7atj 7fd1 7odc 7rsa 8abp 9wga
        """
            |> String.words
            |> Set.fromList
    , metricsRemoteData = initialMetricsRemoteData
    }


pisces : NewReferenceSet
pisces =
    { id = "pisces-subset"
    , name = "Pisces"
    , description =
        """A set of non-redundant structures defined by the
        Dunbrack lab. These structures have an identity cutoff of 20%, a maximum
        resolution of 1.6 Å and a minimum R-work of 25%.  Uses the preferred biological
        unit as defined by PDBe.
        """
    , pdbCodes =
        """
        1a3c 1a62 1ah7 1aho 1amt 1atg 1bgf 1byi 1d5t 1dcs 1dg6 1dj0 1dk8 1dp7 1ds1 1e58
        1elk 1euw 1ezg 1f1e 1f2t 1f46 1f86 1f9v 1g6x 1gci 1gkm 1gmx 1gp0 1gpp 1gv9 1gvp
        1hdo 1hq1 1hxi 1hz4 1i1w 1i27 1i2t 1i4u 1ix9 1j0p 1j2j 1j34 1j3a 1j3w 1j98 1jb3
        1jo0 1jov 1jx6 1jy2 1jyk 1k3x 1k5c 1k5n 1kt6 1kwf 1kyf 1l3k 1l9l 1lc0 1lc5 1lmi
        1m4l 1m55 1m9z 1mc2 1mk0 1mkk 1mn8 1mnn 1nki 1nnx 1nu0 1nwz 1nxm 1nyc 1nz0 1nzj
        1ow4 1oyg 1oz2 1p1x 1p5z 1p6o 1p9g 1p9i 1qg8 1qnr 1qow 1qv1 1qv9 1qw2 1r29 1r5m
        1rju 1rk6 1rki 1rku 1rtq 1rtt 1rv9 1rxi 1sn9 1sqs 1svf 1sx5 1sz7 1szh 1t3y 1t5b
        1tzp 1u07 1u7i 1u84 1ucd 1ucr 1ucs 1ugx 1v05 1v0w 1v6p 1vbw 1vd6 1ve4 1vh5 1vhn
        1vyi 1vyk 1vyr 1vzm 1w0h 1w0n 1w4s 1w53 1wna 1wpa 1ws8 1wt6 1wvq 1wwi 1wy3 1wzd
        1xg0 1xlq 1xmk 1xmt 1xqo 1xub 1y43 1y5h 1yu0 1z0w 1z2n 1z2u 1z67 1z6m 1z6n 1z70
        1zva 1zzk 2a35 2a3n 2a6z 2aac 2abs 2agk 2b4h 2b97 2bay 2bbr 2bdr 2bf9 2bk9 2bkx
        2cc6 2ccq 2ccv 2cg7 2ciu 2cjt 2cov 2cs7 2d1s 2d3d 2d5m 2ddx 2dej 2dho 2dko 2dlb
        2egv 2ehp 2ehz 2end 2erf 2erl 2et1 2ex2 2fb6 2fba 2fcj 2fcl 2fco 2fcw 2fgq 2fhp
        2fsq 2fup 2g3r 2g7o 2g7s 2g84 2gb4 2ggc 2gud 2guh 2gui 2guv 2gyq 2gzs 2h1v 2h30
        2hw2 2hx0 2hx5 2i51 2i53 2i5u 2i5v 2ia1 2imf 2imq 2inw 2ip6 2it2 2iuw 2ixm 2iyv
        2jek 2jfr 2jg0 2jku 2jli 2mcm 2nlv 2nml 2nw8 2nwf 2nwr 2nxv 2o1q 2o2x 2o5g 2o60
        2ofk 2ofz 2ohw 2okf 2okt 2olm 2oln 2oml 2ozj 2ozt 2p0n 2p0s 2p14 2p17 2p4h 2p51
        2pof 2pq7 2pq8 2pr7 2prv 2pxx 2pyq 2q1s 2qfe 2qgu 2qip 2qjl 2qjz 2ql8 2qlt 2qml
        2qud 2qzc 2r01 2r0x 2r16 2r2z 2r31 2r4i 2rhf 2rhw 2ril 2rk9 2rkl 2rl8 2uyt 2uzc
        2v8f 2v8i 2v9v 2vb1 2vc8 2vcl 2vez 2vfr 2vws 2vxn 2vzc 2w15 2w1j 2w1r 2w31 2w39
        2wf7 2wfi 2wfw 2wh6 2wlv 2wnf 2wnk 2wnp 2x3m 2x46 2x49 2x4l 2x4w 2x5o 2x5x 2x5y
        2xol 2xom 2xpw 2xqq 2xry 2xtp 2xw6 2xwv 2y9u 2yc3 2ydt 2yh5 2yln 2ymv 2yn0 2yve
        2zdp 2zfd 2zhj 2znr 2zou 2zpm 2zpt 3a0s 3ach 3acx 3aia 3aj4 3aj7 3ajd 3aks 3alj
        3b79 3b9w 3ba3 3bed 3bf7 3bgu 3bhq 3bhw 3bwh 3bwz 3by8 3c70 3c9a 3cbz 3ccd 3cec
        3clm 3cov 3cp7 3ct5 3ct6 3ctz 3cuz 3cwr 3d7j 3d9n 3d9x 3db7 3dff 3dfg 3dgt 3dha
        3e0x 3e48 3e4g 3e8o 3ef8 3ejf 3ejv 3eki 3f1l 3f2z 3f43 3f6y 3f7e 3fcn 3feg 3fgv
        3fxa 3fym 3fyn 3g02 3g0k 3g21 3g36 3g91 3gkj 3gkr 3gne 3gnl 3gnz 3go5 3goc 3goe
        3gy9 3h0n 3h3l 3h4o 3h4t 3h5j 3h6j 3h74 3hm4 3ho6 3hp7 3hpc 3hr6 3hs3 3hwu 3hx8
        3ie7 3iez 3ife 3ifn 3iis 3imk 3ip0 3ip8 3iwf 3ix3 3jq0 3jrv 3jtz 3jum 3jxo 3jyo
        3kgy 3kh1 3kkf 3kpe 3ktp 3kuv 3kwe 3kwr 3l9a 3laa 3lax 3ld7 3ldc 3lfk 3lfr 3lft
        3lsn 3lti 3lw3 3lwx 3lyd 3m1x 3m3p 3m5q 3md7 3mdq 3mdu 3me7 3mea 3mil 3mjf 3mmh
        3mxn 3mxz 3myx 3n01 
        """
            |> String.words
            |> Set.fromList
    , metricsRemoteData = initialMetricsRemoteData
    }



-- }}}
