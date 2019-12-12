module ReferenceSet exposing (ReferenceSet, ReferenceSetStub)

import Codec exposing (Codec)
import DesignMetrics exposing (DesignMetrics)
import Graphql.Http
import RemoteData as RD exposing (RemoteData)
import Style


type alias ReferenceSet =
    { name : String
    , description : String
    , remoteData : ReferenceSetRemoteData
    , deleteStatus : Style.DangerStatus
    }


type alias ReferenceSetRemoteData =
    RemoteData (Graphql.Http.Error (List DesignMetrics)) (List DesignMetrics)


referenceSetCodec : Codec ReferenceSet
referenceSetCodec =
    Codec.object ReferenceSet
        |> Codec.field "name" .name Codec.string
        |> Codec.field "description" .description Codec.string
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
        |> Codec.buildObject


type alias ReferenceSetStub =
    { name : String
    , description : String
    , deleteStatus : Style.DangerStatus
    }
