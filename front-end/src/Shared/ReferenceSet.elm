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

import BigStructure.Object.BiolUnit as BiolUnit
import BigStructure.Object.Pdb as Pdb
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
        )


preferredStatesSubsetQuery : Set String -> SelectionSet (List Metrics.RefSetMetrics) RootQuery
preferredStatesSubsetQuery pdbCodeList =
    Query.preferredStatesSubset { codes = Set.toList pdbCodeList }
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
