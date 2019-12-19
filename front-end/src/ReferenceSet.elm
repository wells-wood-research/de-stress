module ReferenceSet exposing
    ( ReferenceSet(..)
    , ReferenceSetRemoteData
    , ReferenceSetStub(..)
    , codec
    , createReferenceSetStub
    , getParamsForStub
    , highResBiolUnits
    , queryToCmd
    , referenceSetStubCodec
    )

import BigStructure.Object.State as State
import BigStructure.Query as Query
import Codec exposing (Codec)
import DesignMetrics exposing (DesignMetrics)
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import RemoteData as RD exposing (RemoteData)
import Style



-- {{{ ReferenceSet


type ReferenceSet
    = HighResBiolUnit HighResBiolUnitParams
    | PdbCodeList PdbCodeListParams


type alias HighResBiolUnitParams =
    { metrics : List DesignMetrics
    , deleteStatus : Style.DangerStatus
    }


type alias PdbCodeListParams =
    { name : String
    , description : String
    , pdbCodes : List String
    , metrics : List DesignMetrics
    , deleteStatus : Style.DangerStatus
    }


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
        |> Codec.field "metrics" .metrics (Codec.list DesignMetrics.codec)
        |> Codec.field "deleteStatus" .deleteStatus (Codec.constant Style.Unclicked)
        |> Codec.buildObject


pdbCodeListParamsCodec : Codec PdbCodeListParams
pdbCodeListParamsCodec =
    Codec.object PdbCodeListParams
        |> Codec.field "name" .name Codec.string
        |> Codec.field "description" .description Codec.string
        |> Codec.field "pdbCodes" .pdbCodes (Codec.list Codec.string)
        |> Codec.field "metrics" .metrics (Codec.list DesignMetrics.codec)
        |> Codec.field "deleteStatus" .deleteStatus (Codec.constant Style.Unclicked)
        |> Codec.buildObject



-- }}}
-- {{{ ReferenceSetRemoteData


type alias ReferenceSetRemoteData =
    RemoteData (Graphql.Http.Error (List DesignMetrics)) (List DesignMetrics)


queryToCmd :
    SelectionSet (List DesignMetrics) RootQuery
    -> (ReferenceSetRemoteData -> msg)
    -> Cmd msg
queryToCmd query msgConstructor =
    query
        |> Graphql.Http.queryRequest "http://127.0.0.1:5000/graphql"
        |> Graphql.Http.send (RD.fromResult >> msgConstructor)



-- }}}
-- {{{ Default Reference Sets


type alias DefaultReferenceSet =
    { id : String
    , name : String
    , description : String
    , query : SelectionSet (List DesignMetrics) RootQuery
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


highResBiolMetricQuery : SelectionSet (List DesignMetrics) RootQuery
highResBiolMetricQuery =
    Query.preferredStates
        (\optionals -> { optionals | first = Absent })
        (SelectionSet.map7 DesignMetrics
            (SelectionSet.map DesignMetrics.compositionStringToDict State.composition)
            (SelectionSet.map DesignMetrics.torsionAngleStringToDict State.torsionAngles)
            State.hydrophobicFitness
            State.isoelectricPoint
            State.mass
            State.numOfResidues
            State.meanPackingDensity
        )
        |> SelectionSet.map (List.filterMap identity)



-- }}}
-- {{{ ReferenceSetStub


type ReferenceSetStub
    = HighResBiolUnitStub Style.DangerStatus
    | PdbCodeListStub StubParams


type alias StubParams =
    { name : String
    , description : String
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
        |> Codec.variant1
            "HighResBiolUnitStub"
            HighResBiolUnitStub
            (Codec.constant Style.Unclicked)
        |> Codec.variant1 "PdbCodeListStub" PdbCodeListStub stubParamsCodec
        |> Codec.buildCustom


stubParamsCodec : Codec StubParams
stubParamsCodec =
    Codec.object StubParams
        |> Codec.field "name" .name Codec.string
        |> Codec.field "description" .description Codec.string
        |> Codec.field "deleteStatus" .deleteStatus (Codec.constant Style.Unclicked)
        |> Codec.buildObject


createReferenceSetStub : ReferenceSet -> ReferenceSetStub
createReferenceSetStub referenceSet =
    case referenceSet of
        HighResBiolUnit params ->
            HighResBiolUnitStub params.deleteStatus

        PdbCodeList { name, description, deleteStatus } ->
            PdbCodeListStub
                { name = name
                , description = description
                , deleteStatus =
                    deleteStatus
                }


getParamsForStub : ReferenceSetStub -> StubParams
getParamsForStub referenceSet =
    case referenceSet of
        HighResBiolUnitStub deleteStatus ->
            { name = highResBiolUnits.name
            , description = highResBiolUnits.description
            , deleteStatus = deleteStatus
            }

        PdbCodeListStub params ->
            params



-- }}}
