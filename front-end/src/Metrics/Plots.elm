module Metrics.Plots exposing (histogramView)

import Axis
import Color
import Histogram exposing (Bin)
import Html exposing (Html)
import Metrics exposing (DesignMetrics)
import Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)
import TypedSvg exposing (g, rect, svg)
import TypedSvg.Attributes exposing (class, fill, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width, x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..), Transform(..))


histogram : List Float -> List (Bin Float Float)
histogram model =
    Histogram.float
        |> Histogram.withDomain ( 0, 20 )
        |> Histogram.compute model


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    30


type alias XScaleParameters =
    { width : Float
    , height : Float
    , min : Float
    , max : Float
    , padding : Float
    }


xScale : ContinuousScale Float
xScale =
    Scale.linear ( 0, w - 2 * padding ) ( 0, 20 )


yScaleFromBins : List (Bin Float Float) -> ContinuousScale Float
yScaleFromBins bins =
    List.map .length bins
        |> List.maximum
        |> Maybe.withDefault 0
        |> toFloat
        |> Tuple.pair 0
        |> Scale.linear ( h - 2 * padding, 0 )


xAxis : List Float -> Svg msg
xAxis model =
    Axis.bottom [] xScale


yAxis : List (Bin Float Float) -> Svg msg
yAxis bins =
    Axis.left [ Axis.tickCount 5 ] (yScaleFromBins bins)


column : ContinuousScale Float -> Bin Float Float -> Svg msg
column yScale { length, x0, x1 } =
    rect
        [ x <| Scale.convert xScale x0
        , y <| Scale.convert yScale (toFloat length)
        , width <| Scale.convert xScale x1 - Scale.convert xScale x0
        , height <| h - Scale.convert yScale (toFloat length) - 2 * padding
        , fill <| Paint <| Color.rgb255 46 118 149
        ]
        []


histogramView : List DesignMetrics -> Html msg
histogramView designMetrics =
    let
        data =
            List.map .packingDensity designMetrics

        bins =
            histogram data
    in
    svg [ viewBox 0 0 w h ]
        [ g [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ xAxis data ]
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ yAxis bins ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ] <|
            List.map (column (yScaleFromBins bins)) bins
        ]
