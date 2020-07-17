module Shared.WebSockets exposing (ConnectionStatus, statusIconView, unknownStatus)

import Element exposing (Element)
import FeatherIcons
import Shared.Style as Style


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
