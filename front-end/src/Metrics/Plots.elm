module Metrics.Plots exposing (ColumnData, metricOverview)

import Axis
import Round
import Scale exposing (BandScale, ContinuousScale, defaultBandConfig)
import TypedSvg exposing (g, rect, style, svg, text_)
import TypedSvg.Attributes
    exposing
        ( class
        , dominantBaseline
        , height
        , textAnchor
        , transform
        , viewBox
        , width
        , x
        , y
        )
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Events exposing (onClick)
import TypedSvg.Types
    exposing
        ( AnchorAlignment(..)
        , DominantBaseline(..)
        , Length(..)
        , Transform(..)
        )


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    70


type alias ColumnData =
    { index : Float
    , name : String
    , uuidString : String
    , value : Float
    }


xScale : List ColumnData -> BandScale ColumnData
xScale datapoints =
    Scale.band
        { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 }
        ( 0, w - 2 * padding )
        datapoints


yScale : ContinuousScale Float
yScale =
    Scale.linear ( h - 2 * padding, 0 ) ( 0, 100 )


yAxis : Svg msg
yAxis =
    g []
        [ Axis.left [ Axis.tickCount 5 ] yScale
        , text_
            [ x <| Px -padding
            , textAnchor AnchorStart
            , dominantBaseline DominantBaselineMiddle
            , transform [ Rotate 90 -padding 0 ]
            ]
            [ text "Packing Density" ]
        ]


column : (String -> msg) -> BandScale ColumnData -> ColumnData -> Svg msg
column clickMsg scale ({ value, name, uuidString } as datapoint) =
    g [ class [ "column" ], onClick <| clickMsg uuidString ]
        [ rect
            [ x <| Px <| Scale.convert scale datapoint
            , y <| Px <| Scale.convert yScale value
            , width <| Px <| Scale.bandwidth scale
            , height <| Px <| h - Scale.convert yScale value - 2 * padding
            ]
            []
        , text_
            [ x <| Px <| Scale.convert (Scale.toRenderable .name scale) datapoint
            , y <| Px <| Scale.convert yScale value - 5
            , textAnchor AnchorMiddle
            ]
            [ text <| Round.round 1 value ]
        , text_
            (let
                xVal =
                    Scale.convert (Scale.toRenderable .name scale) datapoint

                yVal =
                    Scale.convert yScale value + 10
             in
             [ x <| Px xVal
             , y <| Px yVal
             , textAnchor AnchorStart
             , dominantBaseline DominantBaselineMiddle
             , transform [ Rotate 90 xVal yVal ]
             ]
            )
            [ text name ]
        ]


metricOverview : (String -> msg) -> List ColumnData -> Svg msg
metricOverview clickMsg data =
    svg [ viewBox 0 0 w h ]
        [ style []
            [ text """
            .tick text { font-size: 24px; }
            .column rect { fill: rgba(118, 214, 78, 0.8); }
            .column text { display: none; }
            .column:hover rect { fill: rgb(118, 214, 78); }
            .column:hover text { display: inline; }
            """
            ]
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ yAxis ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ] <|
            List.map (column clickMsg (xScale data)) data
        ]
