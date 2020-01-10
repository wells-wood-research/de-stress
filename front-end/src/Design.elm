module Design exposing
    ( Design
    , DesignStub
    , Editable(..)
    , codec
    , createDesignStub
    , designStubCodec
    , editableValue
    , metricsRDCodec
    )

import Codec exposing (Codec)
import Graphql.Http
import Metrics exposing (DesignMetrics)
import RemoteData exposing (RemoteData)
import Style
import Utils.RemoteDataExtra as RDE


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
        |> Codec.field "metricsRemoteData" .metricsRemoteData metricsRDCodec
        |> Codec.buildObject


metricsRDCodec : Codec MetricsRemoteData
metricsRDCodec =
    RDE.codec
        (Codec.constant <| Graphql.Http.HttpError Graphql.Http.Timeout)
        Metrics.desMetricsCodec
        |> Debug.log "This shouldn't be timeout"


type alias DesignStub =
    { name : String
    , fileName : String
    , deleteStatus : Style.DangerStatus
    }


designStubCodec : Codec DesignStub
designStubCodec =
    Codec.object DesignStub
        |> Codec.field "name" .name Codec.string
        |> Codec.field "fileName" .fileName Codec.string
        |> Codec.field "deleteStatus" .deleteStatus (Codec.constant Style.Unclicked)
        |> Codec.buildObject


createDesignStub : Design -> DesignStub
createDesignStub { name, fileName, deleteStatus } =
    { name = editableValue name
    , fileName = fileName
    , deleteStatus = deleteStatus
    }
