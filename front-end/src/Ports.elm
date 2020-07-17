port module Ports exposing
    ( MetricsServerJob
    , MetricsServerJobStatus
    , ServerJobStatus(..)
    , WebSocketIncoming(..)
    , WebsocketOutgoing(..)
    , deleteDesign
    , deleteReferenceSet
    , deleteSpecification
    , getDesign
    , getReferenceSet
    , getReferenceSetForDesign
    , getSpecification
    , getSpecificationForDesign
    , getSpecificationForDesignsPage
    , metricsAvailable
    , metricsServerJobCodec
    , metricsServerJobStatusCodec
    , newMetricsServerJob
    , referenceSetForDesign
    , setFocussedDesign
    , setFocussedReferenceSet
    , setFocussedSpecification
    , setWebSocketConnectionStatus
    , specificationForDesign
    , specificationForDesignsPage
    , storeDesign
    , storeReferenceSet
    , storeRunState
    , storeSpecification
    , updateMetricsJobStatus
    , vegaPlot
    , viewStructure
    , webSocketIncoming
    , websocketIncomingCodec
    , websocketOutgoingToCmd
    )

import Codec exposing (Codec, Value)
import Metrics
import VegaLite as VL



-- {{{ Outgoing


port outgoing : { action : String, data : Value } -> Cmd msg



-- Run State


storeRunState : Value -> Cmd msg
storeRunState storedStateValue =
    outgoing
        { action = "STORE_STATE"
        , data = storedStateValue
        }



-- Design


storeDesign : Value -> Cmd msg
storeDesign designAndKey =
    outgoing
        { action = "STORE_DESIGN"
        , data = designAndKey
        }


updateMetricsJobStatus : Value -> Cmd msg
updateMetricsJobStatus metricsStatusAndKey =
    outgoing
        { action = "UPDATE_METRICS_STATUS"
        , data = metricsStatusAndKey
        }


getDesign : Value -> Cmd msg
getDesign uuidValue =
    outgoing
        { action = "GET_DESIGN"
        , data = uuidValue
        }


deleteDesign : Value -> Cmd msg
deleteDesign uuidValue =
    outgoing
        { action = "DELETE_DESIGN"
        , data = uuidValue
        }


viewStructure : Value -> Cmd msg
viewStructure pdbStringValue =
    outgoing
        { action = "VIEW_STRUCTURE"
        , data = pdbStringValue
        }



-- Reference Set


storeReferenceSet : Value -> Cmd msg
storeReferenceSet referenceSetValue =
    outgoing
        { action = "STORE_REFERENCE_SET"
        , data = referenceSetValue
        }


getReferenceSet : Value -> Cmd msg
getReferenceSet uuidValue =
    outgoing
        { action = "GET_REFERENCE_SET"
        , data = uuidValue
        }


getReferenceSetForDesign : Value -> Cmd msg
getReferenceSetForDesign uuidValue =
    outgoing
        { action = "GET_REFERENCE_SET_FOR_DESIGN"
        , data = uuidValue
        }


deleteReferenceSet : Value -> Cmd msg
deleteReferenceSet uuidValue =
    outgoing
        { action = "DELETE_REFERENCE_SET"
        , data = uuidValue
        }



-- Specifications


storeSpecification : Value -> Cmd msg
storeSpecification specValue =
    outgoing
        { action = "STORE_SPECIFICATION"
        , data = specValue
        }


getSpecification : Value -> Cmd msg
getSpecification uuidValue =
    outgoing
        { action = "GET_SPECIFICATION"
        , data = uuidValue
        }


getSpecificationForDesign : Value -> Cmd msg
getSpecificationForDesign uuidValue =
    outgoing
        { action = "GET_SPECIFICATION_FOR_DESIGN"
        , data = uuidValue
        }


getSpecificationForDesignsPage : Value -> Cmd msg
getSpecificationForDesignsPage uuidValue =
    outgoing
        { action = "GET_SPECIFICATION_FOR_DESIGNS_PAGE"
        , data = uuidValue
        }


deleteSpecification : Value -> Cmd msg
deleteSpecification uuidValue =
    outgoing
        { action = "DELETE_SPECIFICATION"
        , data = uuidValue
        }



-- Vega Lite


port vegaPlot : { plotId : String, spec : VL.Spec } -> Cmd msg



-- }}}
-- {{{ Incoming


port setFocussedDesign : (Value -> msg) -> Sub msg


port setFocussedReferenceSet : (Value -> msg) -> Sub msg


port referenceSetForDesign : (Value -> msg) -> Sub msg


port specificationForDesign : (Value -> msg) -> Sub msg


port specificationForDesignsPage : (Value -> msg) -> Sub msg


port setFocussedSpecification : (Value -> msg) -> Sub msg



-- }}}
-- {{{ Websockets


port setWebSocketConnectionStatus : (Value -> msg) -> Sub msg


type WebsocketOutgoing
    = RequestMetrics MetricsServerJob


type alias MetricsServerJob =
    ServerJob RequestMetricsInput Metrics.DesignMetrics


metricsServerJobCodec : Codec MetricsServerJob
metricsServerJobCodec =
    serverJobCodec requestMetricsInputCodec Metrics.desMetricsCodec


type alias RequestMetricsInput =
    { pdbString : String
    }


newMetricsServerJob : { uuid : String, pdbString : String } -> MetricsServerJob
newMetricsServerJob { uuid, pdbString } =
    { uuid = uuid
    , status = Submitted { pdbString = pdbString }
    }


requestMetricsInputCodec : Codec RequestMetricsInput
requestMetricsInputCodec =
    Codec.object
        RequestMetricsInput
        |> Codec.field "pdbString" .pdbString Codec.string
        |> Codec.buildObject


websocketOutgoingCodec : Codec WebsocketOutgoing
websocketOutgoingCodec =
    Codec.custom
        (\fRequestMetrics value ->
            case value of
                RequestMetrics data ->
                    fRequestMetrics data
        )
        |> Codec.variant1 "RequestMetrics"
            RequestMetrics
            (serverJobCodec requestMetricsInputCodec Metrics.desMetricsCodec)
        |> Codec.buildCustom


websocketOutgoingToCmd : WebsocketOutgoing -> Cmd msg
websocketOutgoingToCmd action =
    Codec.encoder websocketOutgoingCodec action
        |> webSocketOutgoing


port webSocketOutgoing : Value -> Cmd msg


port webSocketIncoming : (Value -> msg) -> Sub msg


type WebSocketIncoming
    = ReceivedMetricsJob MetricsServerJob
    | CommunicationError


type alias ServerJob input output =
    { uuid : String
    , status : ServerJobStatus input output
    }


type ServerJobStatus input output
    = Ready
    | Submitted input
    | Queued
    | InProgress
    | Cancelled
    | Failed String
    | Complete output


metricsAvailable : ServerJobStatus a b -> Bool
metricsAvailable serverJobStatus =
    case serverJobStatus of
        Complete _ ->
            True

        _ ->
            False


type alias MetricsServerJobStatus =
    ServerJobStatus RequestMetricsInput Metrics.DesignMetrics


metricsServerJobStatusCodec : Codec MetricsServerJobStatus
metricsServerJobStatusCodec =
    serverJobStatusCodec requestMetricsInputCodec Metrics.desMetricsCodec


websocketIncomingCodec : Codec WebSocketIncoming
websocketIncomingCodec =
    Codec.custom
        (\fReceivedMetricsJob fCommunicationError value ->
            case value of
                ReceivedMetricsJob serverJob ->
                    fReceivedMetricsJob serverJob

                CommunicationError ->
                    fCommunicationError
        )
        |> Codec.variant1 "ReceivedMetricsJob"
            ReceivedMetricsJob
            (serverJobCodec requestMetricsInputCodec Metrics.desMetricsCodec)
        |> Codec.variant0 "CommunicationError"
            CommunicationError
        |> Codec.buildCustom


serverJobCodec : Codec a -> Codec b -> Codec (ServerJob a b)
serverJobCodec codecA codecB =
    Codec.object
        ServerJob
        |> Codec.field "uuid" .uuid Codec.string
        |> Codec.field "status" .status (serverJobStatusCodec codecA codecB)
        |> Codec.buildObject


serverJobStatusCodec : Codec a -> Codec b -> Codec (ServerJobStatus a b)
serverJobStatusCodec codecA codecB =
    Codec.custom
        (\fReady fSubmitted fQueued fInProgress fCancelled fFailed fComplete value ->
            case value of
                Ready ->
                    fReady

                Submitted input ->
                    fSubmitted input

                Queued ->
                    fQueued

                InProgress ->
                    fInProgress

                Cancelled ->
                    fCancelled

                Failed errorString ->
                    fFailed errorString

                Complete output ->
                    fComplete output
        )
        |> Codec.variant0 "Ready" Ready
        |> Codec.variant1 "Submitted" Submitted codecA
        |> Codec.variant0 "Queued" Queued
        |> Codec.variant0 "InProgress" InProgress
        |> Codec.variant0 "Cancelled" Cancelled
        |> Codec.variant1 "Failed" Failed Codec.string
        |> Codec.variant1 "Complete" Complete codecB
        |> Codec.buildCustom



-- }}}
