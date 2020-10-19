port module Shared.Design exposing
    ( Design
    , DesignStub
    , StoredDesign
    , codec
    , createDesignStub
    , deleteAllDesigns
    , deleteDesign
    , designStubCodec
    , getStoredDesign
    , mapStoredDesign
    , storeDesign
    , storeDesignStubLocally
    , storedDesignCodec
    , storedDesignToStub
    , updateDesignMetricsStatus
    , viewStructure
    )

import Codec exposing (Codec, Value)
import Shared.Buttons as Buttons
import Shared.Editable as Editable exposing (Editable)
import Shared.WebSockets as WebSockets



-- {{{ PORTS


port storeDesign : { uuidString : String, design : Value } -> Cmd msg


port getStoredDesign : { uuidString : String } -> Cmd msg


port updateDesignMetricsStatus :
    { uuidString : String, updatedMetricsStatus : Value }
    -> Cmd msg


port deleteDesign : { uuidString : String } -> Cmd msg


port deleteAllDesigns : () -> Cmd msg


port viewStructure : String -> Cmd msg



-- }}}
-- {{{ DESIGN


type alias Design =
    { name : Editable String
    , fileName : String
    , pdbString : String
    , deleteStatus : Buttons.DangerStatus
    , metricsJobStatus : WebSockets.MetricsServerJobStatus
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
        |> Codec.field "deleteStatus"
            .deleteStatus
            (Codec.constant
                Buttons.initDangerStatus
            )
        |> Codec.field "metricsJobStatus"
            .metricsJobStatus
            WebSockets.metricsServerJobStatusCodec
        |> Codec.field "mMeetsActiveSpecification"
            .mMeetsActiveSpecification
            (Codec.constant Nothing)
        |> Codec.buildObject



-- }}}
-- {{{ DESIGN STUB


type alias DesignStub =
    { name : String
    , fileName : String
    , deleteStatus : Buttons.DangerStatus
    , metricsJobStatus : WebSockets.MetricsServerJobStatus
    , mMeetsActiveSpecification : Maybe Bool
    }


designStubCodec : Codec DesignStub
designStubCodec =
    Codec.object DesignStub
        |> Codec.field "name" .name Codec.string
        |> Codec.field "fileName" .fileName Codec.string
        |> Codec.field "deleteStatus" .deleteStatus (Codec.constant Buttons.initDangerStatus)
        |> Codec.field "metricsJobStatus"
            .metricsJobStatus
            WebSockets.metricsServerJobStatusCodec
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



-- }}}
-- {{{ STORED DESIGNS


type StoredDesign
    = LocalDesign DesignStub


storeDesignStubLocally : DesignStub -> StoredDesign
storeDesignStubLocally stub =
    LocalDesign stub


storedDesignCodec : Codec StoredDesign
storedDesignCodec =
    Codec.custom
        (\flocal value ->
            case value of
                LocalDesign stub ->
                    flocal stub
        )
        |> Codec.variant1 "LocalDesign" LocalDesign designStubCodec
        |> Codec.buildCustom


storedDesignToStub : StoredDesign -> DesignStub
storedDesignToStub storedDesign =
    case storedDesign of
        LocalDesign stub ->
            stub


mapStoredDesign : (DesignStub -> DesignStub) -> StoredDesign -> StoredDesign
mapStoredDesign stubFn storedDesign =
    case storedDesign of
        LocalDesign stub ->
            stubFn stub |> LocalDesign



-- }}}
