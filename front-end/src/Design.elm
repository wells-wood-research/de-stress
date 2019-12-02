module Design exposing (Design, DesignStub, Editable(..), codec, createDesignStub)

import Codec exposing (Codec)
import DesignMetrics exposing (DesignMetrics)
import Graphql.Http
import RemoteData as RD exposing (RemoteData)
import Style


type alias Design =
    { name : Editable String
    , fileName : String
    , pdbString : String
    , deleteStatus : Style.DangerStatus
    , metricsRemoteData : MetricsRemoteData
    }


type alias MetricsRemoteData =
    RemoteData (Graphql.Http.Error DesignMetrics) DesignMetrics


type Editable a
    = Editing a (Maybe a)
    | NotEditing a


editableValue : Editable a -> a
editableValue e =
    case e of
        Editing a _ ->
            a

        NotEditing a ->
            a


codec : Codec Design
codec =
    Codec.object Design
        |> Codec.field "name"
            .name
            (Codec.map NotEditing (\a -> editableValue a) Codec.string)
        |> Codec.field "fileName" .fileName Codec.string
        |> Codec.field "pdbString" .pdbString Codec.string
        |> Codec.field "deleteStatus" .deleteStatus (Codec.constant Style.Unclicked)
        |> Codec.field "metricsRemoteData"
            .metricsRemoteData
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
                (Codec.maybe DesignMetrics.codec)
            )
        |> Codec.buildObject


type alias DesignStub =
    { name : String
    , fileName : String
    , metricsAvailable : Bool
    , deleteStatus : Style.DangerStatus
    }


createDesignStub : Design -> DesignStub
createDesignStub { name, fileName, metricsRemoteData, deleteStatus } =
    { name = editableValue name
    , fileName = fileName
    , metricsAvailable =
        case metricsRemoteData of
            RD.Success _ ->
                True

            _ ->
                False
    , deleteStatus = deleteStatus
    }
