port module Shared.ReferenceSet exposing
    ( ReferenceSet
    , ReferenceSetRemoteData
    , ReferenceSetStub
    , StoredReferenceSet
    , codec
    , createReferenceSetStub
    , deleteReferenceSet
    , getReferenceSetForDesignDetails
    , getReferenceSetForRefSetDetails
    , mapStoredReferenceSet
    , preferredStatesSubsetQuery
    , queryToCmd
    , referenceSetStubCodec
    , storeReferenceSet
    , storeReferenceSetStubLocally
    , storedReferenceSetCodec
    , storedReferenceSetToStub
    )

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


type alias ReferenceSet =
    { name : String
    , description : String
    , pdbCodeList : Set String
    , metrics : List RefSetMetrics
    , aggregateData : Metrics.AggregateData
    , deleteStatus : Buttons.DangerStatus
    }


codec : Codec ReferenceSet
codec =
    Codec.object ReferenceSet
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
            |> with (unwrapMSS (State.budeffResults BudeFFResults.totalEnergy))
            |> with (unwrapMSS (State.evoef2Results EvoEF2Results.total))
            |> with (unwrapMSS (State.dfire2Results DFIRE2Results.total))
            |> with (unwrapMSS (State.rosettaResults RosettaResults.totalScore))
            |> with (unwrapMSS (State.aggrescan3dResults Aggrescan3DResults.totalValue))
        )


unwrapMSS : SelectionSet (Maybe (Maybe b)) scope -> SelectionSet (Maybe b) scope
unwrapMSS =
    SelectionSet.map
        (Maybe.andThen identity)



-- }}}
-- {{{ ReferenceSetStub


type alias ReferenceSetStub =
    { name : String
    , description : String
    , aggregateData : Metrics.AggregateData
    , deleteStatus : Buttons.DangerStatus
    }


referenceSetStubCodec : Codec ReferenceSetStub
referenceSetStubCodec =
    Codec.object ReferenceSetStub
        |> Codec.field "name" .name Codec.string
        |> Codec.field "description" .description Codec.string
        |> Codec.field "aggregateData" .aggregateData Metrics.aggregateDataCodec
        |> Codec.field "deleteStatus"
            .deleteStatus
            (Codec.constant Buttons.initDangerStatus)
        |> Codec.buildObject


createReferenceSetStub : ReferenceSet -> ReferenceSetStub
createReferenceSetStub { name, description, aggregateData, deleteStatus } =
    { name = name
    , description = description
    , aggregateData = aggregateData
    , deleteStatus =
        deleteStatus
    }



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
