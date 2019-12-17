module ReferenceSet exposing
    ( ReferenceSet
    , ReferenceSetStub
    , codec
    , createReferenceSetStub
    , generateRemoteDataCmd
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


type alias ReferenceSet =
    { name : String
    , description : String
    , setType : SetType
    , remoteData : ReferenceSetRemoteData
    , deleteStatus : Style.DangerStatus
    }


type SetType
    = AllPrefBiolUnits
    | PdbCodeList (List String)



-- | Pisces PiscesIdentity PiscesResolution PiscesRFactor
-- type PiscesIdentity
--     = Pc20
--     | Pc25
--     | Pc30
--     | Pc40
--     | Pc50
--     | Pc60
--     | Pc70
--     | Pc80
--     | Pc90
-- type PiscesResolution
--     = Res1_6
--     | Res1_8
--     | Res2_0
--     | Res2_2
--     | Res2_5
--     | Res3_0
-- type PiscesRFactor
--     = RF0_25
--     | RF1_0


type alias ReferenceSetRemoteData =
    RemoteData (Graphql.Http.Error (List DesignMetrics)) (List DesignMetrics)


codec : Codec ReferenceSet
codec =
    Codec.object ReferenceSet
        |> Codec.field "name" .name Codec.string
        |> Codec.field "description" .description Codec.string
        |> Codec.field "setType" .setType setTypeCodec
        |> Codec.field "remoteData"
            .remoteData
            (Codec.map
                (\mData ->
                    case mData of
                        Just data ->
                            RD.Success data

                        Nothing ->
                            RD.NotAsked
                )
                (\remoteData ->
                    case remoteData of
                        RD.Success data ->
                            Just data

                        _ ->
                            Nothing
                )
                (Codec.maybe <| Codec.list DesignMetrics.codec)
            )
        |> Codec.field "deleteStatus" .deleteStatus (Codec.constant Style.Unclicked)
        |> Codec.buildObject


setTypeCodec : Codec SetType
setTypeCodec =
    Codec.custom
        (\fallBiol fpdbCodeList value ->
            case value of
                AllPrefBiolUnits ->
                    fallBiol

                PdbCodeList codeList ->
                    fpdbCodeList codeList
        )
        |> Codec.variant0 "AllPrefBiolUnits" AllPrefBiolUnits
        |> Codec.variant1 "PdbCodeList" PdbCodeList (Codec.list Codec.string)
        |> Codec.buildCustom


generateRemoteDataCmd : SetType -> (ReferenceSetRemoteData -> msg) -> Cmd msg
generateRemoteDataCmd setType msgConstructor =
    case setType of
        AllPrefBiolUnits ->
            queryToCmd allPdbMetrics msgConstructor

        PdbCodeList codeList ->
            Debug.todo "Add this"


allPdbMetrics : SelectionSet (List DesignMetrics) RootQuery
allPdbMetrics =
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


queryToCmd :
    SelectionSet (List DesignMetrics) RootQuery
    -> (ReferenceSetRemoteData -> msg)
    -> Cmd msg
queryToCmd query msgConstructor =
    query
        |> Graphql.Http.queryRequest "http://127.0.0.1:5000/graphql"
        |> Graphql.Http.send (RD.fromResult >> msgConstructor)


type alias ReferenceSetStub =
    { name : String
    , description : String
    , deleteStatus : Style.DangerStatus
    }


referenceSetStubCodec : Codec ReferenceSetStub
referenceSetStubCodec =
    Codec.object ReferenceSetStub
        |> Codec.field "name" .name Codec.string
        |> Codec.field "description" .description Codec.string
        |> Codec.field "deleteStatus" .deleteStatus (Codec.constant Style.Unclicked)
        |> Codec.buildObject


createReferenceSetStub : ReferenceSet -> ReferenceSetStub
createReferenceSetStub referenceSet =
    { name = referenceSet.name
    , description = referenceSet.description
    , deleteStatus = referenceSet.deleteStatus
    }
