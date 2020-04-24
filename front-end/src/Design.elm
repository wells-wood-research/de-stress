module Design exposing
    ( Design
    , DesignStub
    , Editable(..)
    , codec
    , createDesignStub
    , designStubCodec
    , editableValue
    )

import Codec exposing (Codec)
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
    { name = editableValue design.name
    , fileName = design.fileName
    , deleteStatus = design.deleteStatus
    , metricsJobStatus = design.metricsJobStatus
    , mMeetsActiveSpecification = design.mMeetsActiveSpecification
    }
