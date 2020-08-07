port module Shared.WebSockets exposing
    ( ConnectionStatus(..)
    , Incoming(..)
    , MetricsServerJob
    , MetricsServerJobStatus
    , Outgoing
    , ServerJobStatus
    , incoming
    , incomingCodec
    , initServerJobStatus
    , metricsAvailable
    , metricsServerJobCodec
    , metricsServerJobStatusCodec
    , newMetricsServerJob
    , prepareMetricsJob
    , setWebSocketConnectionStatus
    , statusIconView
    , unknownStatus
    )

import Codec exposing (Codec, Value)
import Element exposing (..)
import Element.Font as Font
import FeatherIcons
import Shared.Metrics as Metrics
import Shared.Style as Style


port setWebSocketConnectionStatus : (Value -> msg) -> Sub msg


port outgoing : Value -> Cmd msg


port incoming : (Value -> msg) -> Sub msg



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
            FeatherIcons.cloudLightning
                |> Style.featherIconToElmUi
                |> el [ Font.color Style.colorPalette.red ]

        Disconnected ->
            FeatherIcons.cloudOff
                |> Style.featherIconToElmUi
                |> el [ Font.color Style.colorPalette.red ]

        Connected ->
            FeatherIcons.cloud
                |> Style.featherIconToElmUi
                |> el [ Font.color Style.colorPalette.c3 ]



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


initServerJobStatus : ServerJobStatus a b
initServerJobStatus =
    Ready


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
-- {{{ Outgoing


type Outgoing
    = RequestMetrics MetricsServerJob


outgoingRequestMetricsCodec : Codec Outgoing
outgoingRequestMetricsCodec =
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


outgoingToCmd : Outgoing -> Cmd msg
outgoingToCmd action =
    Codec.encoder outgoingRequestMetricsCodec action
        |> outgoing


prepareMetricsJob : { uuidString : String, pdbString : String } -> ( MetricsServerJobStatus, Cmd msg )
prepareMetricsJob { uuidString, pdbString } =
    let
        metricsJob =
            { uuid = uuidString
            , status = Submitted { pdbString = pdbString }
            }
    in
    ( metricsJob.status
    , outgoingToCmd <| RequestMetrics metricsJob
    )



-- }}}
-- {{{ Incoming


type Incoming
    = ReceivedMetricsJob MetricsServerJob
    | CommunicationError


incomingCodec : Codec Incoming
incomingCodec =
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
