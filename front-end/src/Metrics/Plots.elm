module Metrics.Plots exposing (ColumnData, metricOverview)

import Axis
import Color
import Round
import Scale exposing (BandScale, ContinuousScale, defaultBandConfig)
import TypedSvg exposing (g, rect, style, svg, text_)
import TypedSvg.Attributes
    exposing
        ( class
        , dominantBaseline
        , fill
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
        , Paint(..)
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
    , mMeetsSpecification : Maybe Bool
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


yAxis : String -> Svg msg
yAxis yLabel =
    g []
        [ Axis.left [ Axis.tickCount 5 ] yScale
        , text_
            [ x <| Px -padding
            , textAnchor AnchorStart
            , dominantBaseline DominantBaselineMiddle
            , transform [ Rotate 90 -padding 0 ]
            ]
            [ text yLabel ]
        ]


column : (String -> msg) -> BandScale ColumnData -> ColumnData -> Svg msg
column clickMsg scale ({ value, name, uuidString, mMeetsSpecification } as datapoint) =
    g [ class [ "column" ], onClick <| clickMsg uuidString ]
        [ rect
            [ x <| Px <| Scale.convert scale datapoint
            , y <| Px <| Scale.convert yScale value
            , case mMeetsSpecification of
                Just meetsSpecification ->
                    if meetsSpecification then
                        -- #D9B861
                        fill <| Paint <| Color.rgb255 217 184 97

                    else
                        -- #CA6B68
                        fill <| Paint <| Color.rgb255 202 107 104

                Nothing ->
                    -- #D9B861
                    fill <| Paint <| Color.rgb255 217 184 97
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


metricOverview : (String -> msg) -> String -> List ColumnData -> Svg msg
metricOverview clickMsg yLabel data =
    svg [ viewBox 0 0 w h ]
        [ style []
            [ text """
            .tick text { font-size: 24px; }
            .column text { display: none; }
            .column:hover text { display: inline; }
            """
            ]
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ yAxis yLabel ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ] <|
            List.map (column clickMsg (xScale data)) data
        ]
