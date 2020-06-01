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
import TypedSvg.Events as Events
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


yScale : { minValue : Float, maxValue : Float } -> ContinuousScale Float
yScale { minValue, maxValue } =
    let
        buffer =
            (maxValue - minValue) * 0.2
    in
    Scale.linear ( h - 2 * padding, 0 ) ( minValue - buffer, maxValue + buffer )


yAxis : { minValue : Float, maxValue : Float } -> String -> Svg msg
yAxis range yLabel =
    g []
        [ Axis.left [ Axis.tickCount 5 ] <| yScale range
        , text_
            [ x <| Px -padding
            , textAnchor AnchorStart
            , dominantBaseline DominantBaselineMiddle
            , transform [ Rotate 90 -padding 0 ]
            ]
            [ text yLabel ]
        ]


column :
    (String -> msg)
    -> { minValue : Float, maxValue : Float }
    -> BandScale ColumnData
    -> ColumnData
    -> Svg msg
column clickMsg range scale ({ value, name, uuidString, mMeetsSpecification } as datapoint) =
    let
        xval =
            Scale.convert scale datapoint

        yval =
            Scale.convert (yScale range) value
    in
    g
        [ class [ "column" ]
        , Events.onClick <| clickMsg uuidString
        ]
        [ rect
            [ x <| Px xval
            , y <| Px yval
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
            , height <| Px <| h - yval - 2 * padding
            ]
            []
        , text_
            [ x <| Px xval
            , y <| Px <| yval - 40
            , textAnchor AnchorStart
            ]
            [ text name ]
        , text_
            [ x <| Px xval
            , y <| Px <| yval - 15
            , textAnchor AnchorStart
            ]
            [ text <| Round.round 2 value ]
        ]


metricOverview :
    (String -> msg)
    -> String
    -> List ColumnData
    -> Svg msg
metricOverview clickMsg yLabel data =
    let
        values =
            List.map .value data

        minValue =
            List.minimum values |> Maybe.withDefault 0

        maxValue =
            List.maximum values |> Maybe.withDefault 100

        range =
            { minValue = minValue, maxValue = maxValue }
    in
    svg [ viewBox 0 0 w h ]
        [ style []
            [ text styleSheet
            ]
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ yAxis range yLabel ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ] <|
            List.map (column clickMsg range (xScale data)) data
        ]


styleSheet : String
styleSheet =
    """
    .tick text { font-size: 24px; }
    .column { opacity: 0.8; }
    .column:hover { opacity: 1; }
    .column text { display: none; }
    .column:hover text { display: inline; }
    """
