module ReferenceSet exposing
    ( ReferenceSet(..)
    , ReferenceSetRemoteData
    , ReferenceSetStub(..)
    , codec
    , createReferenceSetStub
    , getMetrics
    , getParamsForStub
    , highResBiolUnits
    , queryToCmd
    , referenceSetStubCodec
    )

import BigStructure.Object.State as State
import BigStructure.Query as Query
import Codec exposing (Codec)
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Metrics exposing (RefSetMetrics)
import RemoteData as RD exposing (RemoteData)
import Set exposing (Set)
import Style



-- {{{ ReferenceSet


type ReferenceSet
    = HighResBiolUnit HighResBiolUnitParams
    | PdbCodeList PdbCodeListParams


type alias HighResBiolUnitParams =
    { metrics : List RefSetMetrics
    , aggregateData : Metrics.AggregateData
    , deleteStatus : Style.DangerStatus
    }


type alias PdbCodeListParams =
    { name : String
    , description : String
    , pdbCodeList : Set String
    , metrics : List RefSetMetrics
    , aggregateData : Metrics.AggregateData
    , deleteStatus : Style.DangerStatus
    }


getMetrics : ReferenceSet -> List RefSetMetrics
getMetrics referenceSet =
    case referenceSet of
        HighResBiolUnit { metrics } ->
            metrics

        PdbCodeList { metrics } ->
            metrics


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
        |> Codec.field "deleteStatus" .deleteStatus (Codec.constant Style.Unclicked)
        |> Codec.buildObject


pdbCodeListParamsCodec : Codec PdbCodeListParams
pdbCodeListParamsCodec =
    Codec.object PdbCodeListParams
        |> Codec.field "name" .name Codec.string
        |> Codec.field "description" .description Codec.string
        |> Codec.field "pdbCodeList" .pdbCodeList (Codec.set Codec.string)
        |> Codec.field "metrics" .metrics (Codec.list Metrics.refSetMetricsCodec)
        |> Codec.field "aggregateData" .aggregateData Metrics.aggregateDataCodec
        |> Codec.field "deleteStatus" .deleteStatus (Codec.constant Style.Unclicked)
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
        (SelectionSet.map7 RefSetMetrics
            (SelectionSet.map Metrics.compositionStringToDict State.composition)
            (SelectionSet.map Metrics.torsionAngleStringToDict State.torsionAngles)
            State.hydrophobicFitness
            State.isoelectricPoint
            State.mass
            State.numOfResidues
            State.meanPackingDensity
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
    , deleteStatus : Style.DangerStatus
    }


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
        |> Codec.field "deleteStatus" .deleteStatus (Codec.constant Style.Unclicked)
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
