port module Shared.ReferenceSet exposing
    ( ReferenceSet(..)
    , ReferenceSetRemoteData
    , ReferenceSetStub(..)
    , StoredReferenceSet
    , codec
    , createReferenceSetStub
    , deleteReferenceSet
    , getGenericData
    , getParamsForStub
    , getReferenceSetForDesignDetails
    , getReferenceSetForRefSetDetails
    , highResBiolUnits
    , mapStoredReferenceSet
    , mapStubParams
    , preferredStatesSubsetQuery
    , queryToCmd
    , referenceSetStubCodec
    , storeReferenceSet
    , storeReferenceSetStubLocally
    , storedReferenceSetCodec
    , storedReferenceSetToStub
    , updateDeleteStatus
    )

import BigStructure.Object as Object
import BigStructure.Object.Aggrescan3DResults as Aggrescan3DResults
import BigStructure.Object.BiolUnit as BiolUnit
import BigStructure.Object.BudeFFResults as BudeFFResults
import BigStructure.Object.DFIRE2Results as DFIRE2Results
import BigStructure.Object.EvoEF2Results as EvoEF2Results
import BigStructure.Object.Pdb as Pdb
import BigStructure.Object.RosettaResults as RosettaResults
import BigStructure.Object.State as State
import BigStructure.Query as Query
import Codec exposing (Codec, Value)
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import RemoteData as RD exposing (RemoteData)
import Set exposing (Set)
import Shared.Buttons as Buttons
import Shared.Metrics as Metrics exposing (RefSetMetrics)



-- {{{ PORTS


port storeReferenceSet : { uuidString : String, referenceSet : Value } -> Cmd msg


port getReferenceSetForRefSetDetails : { uuidString : String } -> Cmd msg


port getReferenceSetForDesignDetails : { uuidString : String } -> Cmd msg


port deleteReferenceSet : { uuidString : String } -> Cmd msg



-- }}}
-- {{{ ReferenceSet


type ReferenceSet
    = HighResBiolUnit HighResBiolUnitParams
    | PdbCodeList PdbCodeListParams


type alias HighResBiolUnitParams =
    { metrics : List RefSetMetrics
    , aggregateData : Metrics.AggregateData
    , deleteStatus : Buttons.DangerStatus
    }


type alias PdbCodeListParams =
    { name : String
    , description : String
    , pdbCodeList : Set String
    , metrics : List RefSetMetrics
    , aggregateData : Metrics.AggregateData
    , deleteStatus : Buttons.DangerStatus
    }


type alias GenericData =
    { name : String
    , description : String
    , metrics : List RefSetMetrics
    , aggregateData : Metrics.AggregateData
    , deleteStatus : Buttons.DangerStatus
    }


getGenericData : ReferenceSet -> GenericData
getGenericData referenceSet =
    case referenceSet of
        HighResBiolUnit { metrics, aggregateData, deleteStatus } ->
            { name = highResBiolUnits.name
            , description = highResBiolUnits.description
            , metrics = metrics
            , aggregateData = aggregateData
            , deleteStatus = deleteStatus
            }

        PdbCodeList { name, description, metrics, aggregateData, deleteStatus } ->
            { name = name
            , description = description
            , metrics = metrics
            , aggregateData = aggregateData
            , deleteStatus = deleteStatus
            }


updateDeleteStatus : Buttons.DangerStatus -> ReferenceSet -> ReferenceSet
updateDeleteStatus dangerStatus referenceSet =
    case referenceSet of
        HighResBiolUnit params ->
            HighResBiolUnit { params | deleteStatus = dangerStatus }

        PdbCodeList params ->
            PdbCodeList { params | deleteStatus = dangerStatus }


codec : Codec ReferenceSet
codec =
    Codec.custom
        (\fhighResBiol fpdbCodeList value ->
            case value of
                HighResBiolUnit params ->
                    fhighResBiol params

                PdbCodeList params ->
                    fpdbCodeList params
        )
        |> Codec.variant1 "HighResBiolUnit" HighResBiolUnit highResBiolUnitsParamsCodec
        |> Codec.variant1 "PdbCodeList" PdbCodeList pdbCodeListParamsCodec
        |> Codec.buildCustom


highResBiolUnitsParamsCodec : Codec HighResBiolUnitParams
highResBiolUnitsParamsCodec =
    Codec.object HighResBiolUnitParams
        |> Codec.field "metrics" .metrics (Codec.list Metrics.refSetMetricsCodec)
        |> Codec.field "aggregateData" .aggregateData Metrics.aggregateDataCodec
        |> Codec.field "deleteStatus"
            .deleteStatus
            (Codec.constant Buttons.initDangerStatus)
        |> Codec.buildObject


pdbCodeListParamsCodec : Codec PdbCodeListParams
pdbCodeListParamsCodec =
    Codec.object PdbCodeListParams
        |> Codec.field "name" .name Codec.string
        |> Codec.field "description" .description Codec.string
        |> Codec.field "pdbCodeList" .pdbCodeList (Codec.set Codec.string)
        |> Codec.field "metrics" .metrics (Codec.list Metrics.refSetMetricsCodec)
        |> Codec.field "aggregateData" .aggregateData Metrics.aggregateDataCodec
        |> Codec.field "deleteStatus"
            .deleteStatus
            (Codec.constant Buttons.initDangerStatus)
        |> Codec.buildObject



-- }}}
-- {{{ ReferenceSetRemoteData


type alias ReferenceSetRemoteData =
    RemoteData (Graphql.Http.Error (List RefSetMetrics)) (List RefSetMetrics)


queryToCmd :
    SelectionSet (List RefSetMetrics) RootQuery
    -> (ReferenceSetRemoteData -> msg)
    -> Cmd msg
queryToCmd query msgConstructor =
    query
        |> Graphql.Http.queryRequest "http://127.0.0.1:8181/graphql"
        |> Graphql.Http.send (RD.fromResult >> msgConstructor)



-- }}}
-- {{{ Default Reference Sets


type alias DefaultReferenceSet =
    { id : String
    , name : String
    , description : String
    , query : SelectionSet (List RefSetMetrics) RootQuery
    }


highResBiolUnits : DefaultReferenceSet
highResBiolUnits =
    { id = "high-res-biol-units"
    , name = "High Res Biol Units"
    , description =
        """A set of high-resolution, non-redundant protein structures. Uses the
        preferred biological unit as defined by PDBe."""
    , query = highResBiolMetricQuery
    }


highResBiolMetricQuery : SelectionSet (List RefSetMetrics) RootQuery
highResBiolMetricQuery =
    Query.preferredStates
        (\optionals -> { optionals | first = Absent })
        (SelectionSet.succeed RefSetMetrics
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
            |> with budeffResultsSelectionSet
            |> with evoef2ResultsSelectionSet
            |> with dfire2ResultsSelectionSet
            |> with rosettaResultsSelectionSet
            |> with aggrescan3dResultsSelectionSet
        )


preferredStatesSubsetQuery : Set String -> SelectionSet (List Metrics.RefSetMetrics) RootQuery
preferredStatesSubsetQuery pdbCodeList =
    Query.preferredStatesSubset
        (\optionals -> { optionals | stateNumber = Absent })
        { codes = Set.toList pdbCodeList }
        (SelectionSet.succeed Metrics.RefSetMetrics
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
            |> with budeffResultsSelectionSet
            |> with evoef2ResultsSelectionSet
            |> with dfire2ResultsSelectionSet
            |> with rosettaResultsSelectionSet
            |> with aggrescan3dResultsSelectionSet
        )


unwrapMSS : SelectionSet (Maybe (Maybe b)) scope -> SelectionSet (Maybe b) scope
unwrapMSS =
    SelectionSet.map
        (Maybe.andThen identity)


budeffResultsSelectionSet : SelectionSet (Maybe Metrics.BudeFFResults) Object.State
budeffResultsSelectionSet =
    SelectionSet.map Just
        (SelectionSet.succeed Metrics.BudeFFResults
            |> with (unwrapMSS (State.budeffResults BudeFFResults.totalEnergy))
            |> with (unwrapMSS (State.budeffResults BudeFFResults.steric))
            |> with (unwrapMSS (State.budeffResults BudeFFResults.desolvation))
            |> with (unwrapMSS (State.budeffResults BudeFFResults.charge))
        )


evoef2ResultsSelectionSet : SelectionSet (Maybe Metrics.EvoEF2Results) Object.State
evoef2ResultsSelectionSet =
    SelectionSet.map Just
        (SelectionSet.succeed Metrics.EvoEF2Results
            |> with
                (SelectionSet.map (Maybe.withDefault "--")
                    (State.evoef2Results
                        EvoEF2Results.logInfo
                    )
                )
            |> with
                (SelectionSet.map (Maybe.withDefault "--")
                    (State.evoef2Results
                        EvoEF2Results.errorInfo
                    )
                )
            |> with
                (SelectionSet.map (Maybe.withDefault -1)
                    (State.evoef2Results
                        EvoEF2Results.returnCode
                    )
                )
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.referenceAla))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.referenceCys))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.referenceAsp))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.referenceGlu))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.referencePhe))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.referenceGly))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.referenceHis))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.referenceIle))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.referenceLys))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.referenceLeu))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.referenceMet))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.referenceAsn))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.referencePro))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.referenceGln))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.referenceArg))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.referenceSer))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.referenceThr))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.referenceVal))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.referenceTrp))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.referenceTyr))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.intraRVdwatt))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.intraRVdwrep))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.intraRElectr))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.intraRDeslvp))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.intraRDeslvh))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.intraRHbscbbDis))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.intraRHbscbbThe))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.intraRHbscbbPhi))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.aapropensity))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.ramachandran))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.dunbrack))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.interSVdwatt))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.interSVdwrep))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.interSElectr))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.interSDeslvp))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.interSDeslvh))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.interSSsbond))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.interSHbbbbbDis))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.interSHbbbbbThe))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.interSHbbbbbPhi))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.interSHbscbbDis))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.interSHbscbbThe))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.interSHbscbbPhi))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.interSHbscscDis))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.interSHbscscThe))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.interSHbscscPhi))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.interDVdwatt))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.interDVdwrep))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.interDElectr))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.interDDeslvp))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.interDDeslvh))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.interDSsbond))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.interDHbbbbbDis))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.interDHbbbbbThe))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.interDHbbbbbPhi))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.interDHbscbbDis))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.interDHbscbbThe))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.interDHbscbbPhi))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.interDHbscscDis))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.interDHbscscThe))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.interDHbscscPhi))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.total))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.refTotal))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.intraRTotal))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.interSTotal))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.interDTotal))
        )


dfire2ResultsSelectionSet : SelectionSet (Maybe Metrics.DFIRE2Results) Object.State
dfire2ResultsSelectionSet =
    SelectionSet.map Just
        (SelectionSet.succeed Metrics.DFIRE2Results
            |> with
                (SelectionSet.map (Maybe.withDefault "--")
                    (State.dfire2Results
                        DFIRE2Results.logInfo
                    )
                )
            |> with
                (SelectionSet.map (Maybe.withDefault "--")
                    (State.dfire2Results
                        DFIRE2Results.errorInfo
                    )
                )
            |> with
                (SelectionSet.map (Maybe.withDefault -1)
                    (State.dfire2Results
                        DFIRE2Results.returnCode
                    )
                )
            |> with (unwrapMSS (State.dfire2Results DFIRE2Results.total))
        )


rosettaResultsSelectionSet : SelectionSet (Maybe Metrics.RosettaResults) Object.State
rosettaResultsSelectionSet =
    SelectionSet.map Just
        (SelectionSet.succeed Metrics.RosettaResults
            |> with
                (SelectionSet.map (Maybe.withDefault "--")
                    (State.rosettaResults RosettaResults.logInfo)
                )
            |> with
                (SelectionSet.map (Maybe.withDefault "--")
                    (State.rosettaResults RosettaResults.errorInfo)
                )
            |> with
                (SelectionSet.map (Maybe.withDefault -1)
                    (State.rosettaResults RosettaResults.returnCode)
                )
            |> with (unwrapMSS (State.rosettaResults RosettaResults.dslfFa13))
            |> with (unwrapMSS (State.rosettaResults RosettaResults.faAtr))
            |> with (unwrapMSS (State.rosettaResults RosettaResults.faDun))
            |> with (unwrapMSS (State.rosettaResults RosettaResults.faElec))
            |> with (unwrapMSS (State.rosettaResults RosettaResults.faIntraRep))
            |> with (unwrapMSS (State.rosettaResults RosettaResults.faIntraSolXover4))
            |> with (unwrapMSS (State.rosettaResults RosettaResults.faRep))
            |> with (unwrapMSS (State.rosettaResults RosettaResults.faSol))
            |> with (unwrapMSS (State.rosettaResults RosettaResults.hbondBbSc))
            |> with (unwrapMSS (State.rosettaResults RosettaResults.hbondLrBb))
            |> with (unwrapMSS (State.rosettaResults RosettaResults.hbondSc))
            |> with (unwrapMSS (State.rosettaResults RosettaResults.hbondSrBb))
            |> with (unwrapMSS (State.rosettaResults RosettaResults.linearChainbreak))
            |> with (unwrapMSS (State.rosettaResults RosettaResults.lkBallWtd))
            |> with (unwrapMSS (State.rosettaResults RosettaResults.omega))
            |> with (unwrapMSS (State.rosettaResults RosettaResults.overlapChainbreak))
            |> with (unwrapMSS (State.rosettaResults RosettaResults.pAaPp))
            |> with (unwrapMSS (State.rosettaResults RosettaResults.proClose))
            |> with (unwrapMSS (State.rosettaResults RosettaResults.ramaPrepro))
            |> with (unwrapMSS (State.rosettaResults RosettaResults.ref))
            |> with (unwrapMSS (State.rosettaResults RosettaResults.score))
            |> with (unwrapMSS (State.rosettaResults RosettaResults.time))
            |> with (unwrapMSS (State.rosettaResults RosettaResults.totalScore))
            |> with (unwrapMSS (State.rosettaResults RosettaResults.yhhPlanarity))
        )


aggrescan3dResultsSelectionSet : SelectionSet (Maybe Metrics.Aggrescan3DResults) Object.State
aggrescan3dResultsSelectionSet =
    SelectionSet.map Just
        (SelectionSet.succeed Metrics.Aggrescan3DResults
            |> with
                (SelectionSet.map (Maybe.withDefault "--")
                    (State.aggrescan3dResults Aggrescan3DResults.logInfo)
                )
            |> with
                (SelectionSet.map (Maybe.withDefault "--")
                    (State.aggrescan3dResults Aggrescan3DResults.errorInfo)
                )
            |> with
                (SelectionSet.map (Maybe.withDefault -1)
                    (State.aggrescan3dResults Aggrescan3DResults.returnCode)
                )
            |> with (unwrapMSS (State.aggrescan3dResults Aggrescan3DResults.proteinList))
            |> with (unwrapMSS (State.aggrescan3dResults Aggrescan3DResults.chainList))
            |> with (unwrapMSS (State.aggrescan3dResults Aggrescan3DResults.residueNumberList))
            |> with (unwrapMSS (State.aggrescan3dResults Aggrescan3DResults.residueNameList))
            |> with (unwrapMSS (State.aggrescan3dResults Aggrescan3DResults.residueScoreList))
            |> with (unwrapMSS (State.aggrescan3dResults Aggrescan3DResults.maxValue))
            |> with (unwrapMSS (State.aggrescan3dResults Aggrescan3DResults.avgValue))
            |> with (unwrapMSS (State.aggrescan3dResults Aggrescan3DResults.minValue))
            |> with (unwrapMSS (State.aggrescan3dResults Aggrescan3DResults.totalValue))
        )



-- }}}
-- {{{ ReferenceSetStub


type ReferenceSetStub
    = HighResBiolUnitStub StubParams
    | PdbCodeListStub StubParams


type alias StubParams =
    { name : String
    , description : String
    , aggregateData : Metrics.AggregateData
    , deleteStatus : Buttons.DangerStatus
    }


mapStubParams : (StubParams -> StubParams) -> ReferenceSetStub -> ReferenceSetStub
mapStubParams fn stub =
    case stub of
        HighResBiolUnitStub params ->
            fn params
                |> HighResBiolUnitStub

        PdbCodeListStub params ->
            fn params
                |> PdbCodeListStub


referenceSetStubCodec : Codec ReferenceSetStub
referenceSetStubCodec =
    Codec.custom
        (\fhighResBiol fpdbCodeList value ->
            case value of
                HighResBiolUnitStub params ->
                    fhighResBiol params

                PdbCodeListStub params ->
                    fpdbCodeList params
        )
        |> Codec.variant1 "HighResBiolUnitStub"
            HighResBiolUnitStub
            stubParamsCodec
        |> Codec.variant1 "PdbCodeListStub" PdbCodeListStub stubParamsCodec
        |> Codec.buildCustom


stubParamsCodec : Codec StubParams
stubParamsCodec =
    Codec.object StubParams
        |> Codec.field "name" .name Codec.string
        |> Codec.field "description" .description Codec.string
        |> Codec.field "aggregateData" .aggregateData Metrics.aggregateDataCodec
        |> Codec.field "deleteStatus"
            .deleteStatus
            (Codec.constant Buttons.initDangerStatus)
        |> Codec.buildObject


createReferenceSetStub : ReferenceSet -> ReferenceSetStub
createReferenceSetStub referenceSet =
    case referenceSet of
        HighResBiolUnit { deleteStatus, aggregateData } ->
            HighResBiolUnitStub
                { name = highResBiolUnits.name
                , description = highResBiolUnits.description
                , aggregateData = aggregateData
                , deleteStatus = deleteStatus
                }

        PdbCodeList { name, description, aggregateData, deleteStatus } ->
            PdbCodeListStub
                { name = name
                , description = description
                , aggregateData = aggregateData
                , deleteStatus =
                    deleteStatus
                }


getParamsForStub : ReferenceSetStub -> StubParams
getParamsForStub referenceSet =
    case referenceSet of
        HighResBiolUnitStub { deleteStatus, aggregateData } ->
            { name = highResBiolUnits.name
            , description = highResBiolUnits.description
            , aggregateData = aggregateData
            , deleteStatus = deleteStatus
            }

        PdbCodeListStub params ->
            params



-- }}}
-- {{{ StoredReferenceSet


type StoredReferenceSet
    = LocalReferenceSet ReferenceSetStub


storeReferenceSetStubLocally : ReferenceSetStub -> StoredReferenceSet
storeReferenceSetStubLocally stub =
    LocalReferenceSet stub


mapStoredReferenceSet :
    (ReferenceSetStub -> ReferenceSetStub)
    -> StoredReferenceSet
    -> StoredReferenceSet
mapStoredReferenceSet stubFn storedReferenceSet =
    case storedReferenceSet of
        LocalReferenceSet stub ->
            stubFn stub |> LocalReferenceSet


storedReferenceSetToStub : StoredReferenceSet -> ReferenceSetStub
storedReferenceSetToStub storedReferenceSet =
    case storedReferenceSet of
        LocalReferenceSet stub ->
            stub


storedReferenceSetCodec : Codec StoredReferenceSet
storedReferenceSetCodec =
    Codec.custom
        (\flocal value ->
            case value of
                LocalReferenceSet stub ->
                    flocal stub
        )
        |> Codec.variant1 "LocalReferenceSet"
            LocalReferenceSet
            referenceSetStubCodec
        |> Codec.buildCustom



-- }}}
