module Layout exposing (view)

import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Region as Region
import FeatherIcons
import Generated.Routes as Routes exposing (Route, routes)
import Global
import Style exposing (colorPalette)
import Utils.Spa as Spa


view : Spa.LayoutContext msg -> Element msg
view { global, page, route } =
    column
        [ height fill
        , width fill
        , Font.family
            [ Font.typeface "Roboto"
            , Font.sansSerif
            ]
        ]
        [ case global of
            Global.FailedToLaunch _ ->
                viewHeader route Global.Unknown

            Global.Running { webSocketConnectionStatus } ->
                viewHeader route webSocketConnectionStatus
        , el
            [ centerX
            , paddingXY 50 30
            , spacing 30
            , width (fill |> maximum 800)
            ]
            page
        ]


viewHeader : Route -> Global.WebSocketConnectionStatus -> Element msg
viewHeader currentRoute connStat =
    row
        [ paddingXY 5 0
        , spacing 10
        , width fill
        , Background.color colorPalette.c1
        , Font.color colorPalette.c4
        , Font.size 32
        , Font.bold
        , Region.navigation
        ]
        [ Style.h1 <| link [] { url = "/", label = text "DE-STRESS" }
        , el []
            (case connStat of
                Global.Unknown ->
                    FeatherIcons.cloudLightning |> Style.featherIconToElmUi

                Global.Disconnected ->
                    FeatherIcons.cloudOff |> Style.featherIconToElmUi

                Global.Connected ->
                    FeatherIcons.cloud |> Style.featherIconToElmUi
            )
        , row
            [ alignRight
            , spacingXY 10 0
            , Font.medium
            , Font.size 24
            ]
            ([ viewLink currentRoute ( text "Designs", routes.designs )
             , viewLink currentRoute ( text "Reference Sets", routes.referenceSets )
             , viewLink currentRoute ( text "Specifications", routes.specifications )
             , viewLink currentRoute
                ( FeatherIcons.settings
                    |> Style.featherIconToElmUi
                , routes.notFound
                )
             ]
                |> List.intersperse (el [ Font.color colorPalette.c4 ] <| text "|")
            )
        ]


viewLink : Route -> ( Element msg, Route ) -> Element msg
viewLink currentRoute ( label, route ) =
    if currentRoute == route then
        el
            []
            label

    else
        link
            [ alpha 0.5
            , mouseOver [ alpha 1 ]
            ]
            { label = label
            , url = Routes.toPath route
            }
