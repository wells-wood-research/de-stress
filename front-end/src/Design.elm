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
import RemoteData as RD exposing (RemoteData)
import Style
import Utils.RemoteDataExtra as RDE


type alias Design =
    { name : Editable String
    , fileName : String
    , pdbString : String
    , deleteStatus : Style.DangerStatus
    , metricsRemoteData : MetricsRemoteData
    , mMeetsActiveSpecification : Maybe Bool
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
        |> Codec.field "mMeetsActiveSpecification"
            .mMeetsActiveSpecification
            (Codec.constant Nothing)
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
    , metricsAvailable : Bool
    , mMeetsActiveSpecification : Maybe Bool
    }


designStubCodec : Codec DesignStub
designStubCodec =
    Codec.object DesignStub
        |> Codec.field "name" .name Codec.string
        |> Codec.field "fileName" .fileName Codec.string
        |> Codec.field "deleteStatus" .deleteStatus (Codec.constant Style.Unclicked)
        |> Codec.field "metricsAvailable" .metricsAvailable Codec.bool
        |> Codec.field "mMeetsActiveSpecification"
            .mMeetsActiveSpecification
            (Codec.constant Nothing)
        |> Codec.buildObject


createDesignStub : Design -> DesignStub
createDesignStub design =
    { name = editableValue design.name
    , fileName = design.fileName
    , deleteStatus = design.deleteStatus
    , metricsAvailable =
        case design.metricsRemoteData of
            RD.Success _ ->
                True

            _ ->
                False
    , mMeetsActiveSpecification = design.mMeetsActiveSpecification
    }
