module Design exposing
    ( Design
    , DesignStub
    , codec
    , createDesignStub
    , designStubCodec
    )

import Codec exposing (Codec)
import Editable exposing (Editable)
import Ports
import Style


type alias Design =
    { name : Editable String
    , fileName : String
    , pdbString : String
    , deleteStatus : Style.DangerStatus
    , metricsJobStatus : Ports.MetricsServerJobStatus
    , mMeetsActiveSpecification : Maybe Bool
    }


codec : Codec Design
codec =
    Codec.object Design
        |> Codec.field "name"
            .name
            (Codec.map Editable.NotEditing (\a -> Editable.editableValue a) Codec.string)
        |> Codec.field "fileName" .fileName Codec.string
        |> Codec.field "pdbString" .pdbString Codec.string
        |> Codec.field "deleteStatus" .deleteStatus (Codec.constant Style.Unclicked)
        |> Codec.field "metricsJobStatus" .metricsJobStatus Ports.metricsServerJobStatusCodec
        |> Codec.field "mMeetsActiveSpecification"
            .mMeetsActiveSpecification
            (Codec.constant Nothing)
        |> Codec.buildObject


type alias DesignStub =
    { name : String
    , fileName : String
    , deleteStatus : Style.DangerStatus
    , metricsJobStatus : Ports.MetricsServerJobStatus
    , mMeetsActiveSpecification : Maybe Bool
    }


designStubCodec : Codec DesignStub
designStubCodec =
    Codec.object DesignStub
        |> Codec.field "name" .name Codec.string
        |> Codec.field "fileName" .fileName Codec.string
        |> Codec.field "deleteStatus" .deleteStatus (Codec.constant Style.Unclicked)
        |> Codec.field "metricsJobStatus" .metricsJobStatus Ports.metricsServerJobStatusCodec
        |> Codec.field "mMeetsActiveSpecification"
            .mMeetsActiveSpecification
            (Codec.constant Nothing)
        |> Codec.buildObject


createDesignStub : Design -> DesignStub
createDesignStub design =
    { name = Editable.editableValue design.name
    , fileName = design.fileName
    , deleteStatus = design.deleteStatus
    , metricsJobStatus = design.metricsJobStatus
    , mMeetsActiveSpecification = design.mMeetsActiveSpecification
    }
