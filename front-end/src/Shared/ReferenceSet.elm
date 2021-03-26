port module Shared.ReferenceSet exposing
    ( ReferenceSet
    , ReferenceSetStub
    , StoredReferenceSet
    , codec
    , createReferenceSetStub
    , deleteReferenceSet
    , getReferenceSetForDesignDetails
    , getReferenceSetForRefSetDetails
    , mapStoredReferenceSet
    , referenceSetStubCodec
    , storeReferenceSet
    , storeReferenceSetStubLocally
    , storedReferenceSetCodec
    , storedReferenceSetToStub
    )

import Codec exposing (Codec, Value)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Set exposing (Set)
import Shared.Buttons as Buttons
import Shared.Metrics as Metrics exposing (RefSetMetrics)
import Shared.Tooltips exposing (HoverInfoOption(..))



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
