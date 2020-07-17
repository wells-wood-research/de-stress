port module Shared.WebSockets exposing
    ( ConnectionStatus
    , MetricsServerJob
    , MetricsServerJobStatus
    , ServerJobStatus
    , WebSocketIncoming
    , WebsocketOutgoing
    , metricsAvailable
    , metricsServerJobCodec
    , metricsServerJobStatusCodec
    , newMetricsServerJob
    , setWebSocketConnectionStatus
    , statusIconView
    , unknownStatus
    , webSocketIncoming
    , websocketIncomingCodec
    , websocketOutgoingToCmd
    )

import Codec exposing (Codec, Value)
import Element exposing (Element)
import FeatherIcons
import Shared.Metrics as Metrics
import Shared.Style as Style


port setWebSocketConnectionStatus : (Value -> msg) -> Sub msg


port webSocketOutgoing : Value -> Cmd msg


port webSocketIncoming : (Value -> msg) -> Sub msg



-- {{{ ConnectionStatus


type ConnectionStatus
    = Unknown
    | Disconnected
    | Connected


unknownStatus : ConnectionStatus
unknownStatus =
    Unknown


statusIconView : ConnectionStatus -> Element msg
statusIconView status =
    case status of
        Unknown ->
            FeatherIcons.cloudLightning |> Style.featherIconToElmUi

        Disconnected ->
            FeatherIcons.cloudOff |> Style.featherIconToElmUi

        Connected ->
            FeatherIcons.cloud |> Style.featherIconToElmUi



-- }}}
-- {{{ ServerJob


type alias ServerJob input output =
    { uuid : String
    , status : ServerJobStatus input output
    }


serverJobCodec : Codec a -> Codec b -> Codec (ServerJob a b)
serverJobCodec codecA codecB =
    Codec.object
        ServerJob
        |> Codec.field "uuid" .uuid Codec.string
        |> Codec.field "status" .status (serverJobStatusCodec codecA codecB)
        |> Codec.buildObject


type alias MetricsServerJob =
    ServerJob RequestMetricsInput Metrics.DesignMetrics


type alias RequestMetricsInput =
    { pdbString : String
    }


newMetricsServerJob : { uuid : String, pdbString : String } -> MetricsServerJob
newMetricsServerJob { uuid, pdbString } =
    { uuid = uuid
    , status = Submitted { pdbString = pdbString }
    }


metricsServerJobCodec : Codec MetricsServerJob
metricsServerJobCodec =
    serverJobCodec requestMetricsInputCodec Metrics.desMetricsCodec


requestMetricsInputCodec : Codec RequestMetricsInput
requestMetricsInputCodec =
    Codec.object
        RequestMetricsInput
        |> Codec.field "pdbString" .pdbString Codec.string
        |> Codec.buildObject



-- }}}
-- {{{ ServerJobStatus


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


type alias MetricsServerJobStatus =
    ServerJobStatus RequestMetricsInput Metrics.DesignMetrics


metricsServerJobStatusCodec : Codec MetricsServerJobStatus
metricsServerJobStatusCodec =
    serverJobStatusCodec requestMetricsInputCodec Metrics.desMetricsCodec



-- }}}
-- {{{ WebsocketOutgoing


type WebsocketOutgoing
    = RequestMetrics MetricsServerJob


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



-- }}}
-- {{{ WebSocketIncoming


type WebSocketIncoming
    = ReceivedMetricsJob MetricsServerJob
    | CommunicationError


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



-- }}}
