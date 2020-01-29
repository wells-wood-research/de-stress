module Metrics exposing
    ( DesMetricsRemoteData
    , DesignMetrics
    , RefSetMetrics
    , RefSetMetricsRemoteData
    , compositionStringToDict
    , createAllHistogramsSpec
    , createCompositionSpec
    , createTorsionAngleSpec
    , desMetRemoteDataView
    , desMetricsCodec
    , refSetMetricsCodec
    , torsionAngleStringToDict
    )

import Codec exposing (Codec)
import Dict exposing (Dict)
import Element exposing (..)
import Graphql.Http
import Parser exposing ((|.), (|=))
import RemoteData as RD exposing (RemoteData)
import Utils.ListExtra as ListExtra
import VegaLite as VL


type alias DesignMetrics =
    { sequences : Dict String String
    , composition : Dict String Float
    , torsionAngles : Dict String ( Float, Float, Float )
    , hydrophobicFitness : Maybe Float
    , isoelectricPoint : Float
    , mass : Float
    , numOfResidues : Int
    , packingDensity : Float
    }


desMetricsCodec : Codec DesignMetrics
desMetricsCodec =
    Codec.object DesignMetrics
        |> Codec.field "sequences" .sequences (Codec.dict Codec.string)
        |> Codec.field "composition" .composition (Codec.dict Codec.float)
        |> Codec.field "torsionAngles"
            .torsionAngles
            (Codec.dict
                (Codec.triple
                    Codec.float
                    Codec.float
                    Codec.float
                )
            )
        |> Codec.field "hydrophobicFitness" .hydrophobicFitness (Codec.maybe Codec.float)
        |> Codec.field "isoelectricPoint" .isoelectricPoint Codec.float
        |> Codec.field "mass" .mass Codec.float
        |> Codec.field "numOfResidues" .numOfResidues Codec.int
        |> Codec.field "packingDensity" .packingDensity Codec.float
        |> Codec.buildObject


type alias DesMetricsRemoteData =
    RemoteData (Graphql.Http.Error DesignMetrics) DesignMetrics


type alias RefSetMetrics =
    { composition : Dict String Float
    , torsionAngles : Dict String ( Float, Float, Float )
    , hydrophobicFitness : Maybe Float
    , isoelectricPoint : Float
    , mass : Float
    , numOfResidues : Int
    , packingDensity : Float
    }


refSetMetricsCodec : Codec RefSetMetrics
refSetMetricsCodec =
    Codec.object RefSetMetrics
        |> Codec.field "composition" .composition (Codec.dict Codec.float)
        |> Codec.field "torsionAngles"
            .torsionAngles
            (Codec.dict
                (Codec.triple
                    Codec.float
                    Codec.float
                    Codec.float
                )
            )
        |> Codec.field "hydrophobicFitness" .hydrophobicFitness (Codec.maybe Codec.float)
        |> Codec.field "isoelectricPoint" .isoelectricPoint Codec.float
        |> Codec.field "mass" .mass Codec.float
        |> Codec.field "numOfResidues" .numOfResidues Codec.int
        |> Codec.field "packingDensity" .packingDensity Codec.float
        |> Codec.buildObject


type alias RefSetMetricsRemoteData =
    RemoteData (Graphql.Http.Error RefSetMetrics) RefSetMetrics



-- {{{ Helper Functions


compositionStringToDict : String -> Dict String Float
compositionStringToDict compString =
    Parser.run compositionParser compString
        |> Result.map Dict.fromList
        |> Result.withDefault Dict.empty


compositionParser : Parser.Parser (List ( String, Float ))
compositionParser =
    let
        residueHelp residues =
            Parser.oneOf
                [ Parser.succeed (\residue -> Parser.Loop (residue :: residues))
                    |= residueFrequencyParser
                    |. Parser.oneOf
                        [ Parser.symbol ";"
                        , Parser.end
                        ]
                , Parser.succeed ()
                    |> Parser.map (\_ -> Parser.Done residues)
                ]
    in
    Parser.loop [] residueHelp


residueFrequencyParser : Parser.Parser ( String, Float )
residueFrequencyParser =
    Parser.succeed Tuple.pair
        |= (Parser.getChompedString <|
                Parser.succeed ()
                    |. Parser.chompWhile (\c -> Char.isAlphaNum c)
           )
        |. Parser.symbol ":"
        |= Parser.float


torsionAngleStringToDict : String -> Dict String ( Float, Float, Float )
torsionAngleStringToDict torsionAnglesString =
    torsionAnglesString
        |> Parser.run torsionAnglesParser
        |> Result.map Dict.fromList
        |> Result.withDefault Dict.empty


torsionAnglesParser : Parser.Parser (List ( String, ( Float, Float, Float ) ))
torsionAnglesParser =
    let
        torsionAnglesHelp tas =
            Parser.oneOf
                [ Parser.succeed (\ta -> Parser.Loop (ta :: tas))
                    |= torsionAngleParser
                    |. Parser.oneOf
                        [ Parser.symbol ")"
                        , Parser.end
                        ]
                , Parser.succeed ()
                    |> Parser.map (\_ -> Parser.Done tas)
                ]
    in
    Parser.loop [] torsionAnglesHelp


torsionAngleParser : Parser.Parser ( String, ( Float, Float, Float ) )
torsionAngleParser =
    let
        negativeFloatParser =
            Parser.oneOf
                [ Parser.succeed negate
                    |. Parser.symbol "-"
                    |= Parser.float
                , Parser.float
                ]
    in
    Parser.succeed (\id om ph ps -> ( id, ( om, ph, ps ) ))
        |= (Parser.getChompedString <|
                Parser.succeed ()
                    |. Parser.chompWhile (\c -> Char.isAlphaNum c || c == '_')
           )
        |. Parser.symbol "("
        |= negativeFloatParser
        |. Parser.symbol ","
        |= negativeFloatParser
        |. Parser.symbol ","
        |= negativeFloatParser



-- }}}
-- {{{ Views


desMetRemoteDataView :
    (DesignMetrics -> Element msg)
    -> DesMetricsRemoteData
    -> Element msg
desMetRemoteDataView successView remoteData =
    case remoteData of
        RD.NotAsked ->
            text "Metrics have not been requested to run, try uploading the design again."

        RD.Loading ->
            text "Metrics requested, awaiting results..."

        RD.Failure error ->
            failureView error

        RD.Success metricsRemoteData ->
            successView metricsRemoteData


failureView : Graphql.Http.Error DesignMetrics -> Element msg
failureView error =
    case error of
        Graphql.Http.GraphqlError _ errorList ->
            textColumn []
                (paragraph []
                    [ text
                        "One or more errors occurred while analysing the structure:"
                    ]
                    :: (List.map .message errorList
                            |> List.map (\e -> paragraph [] [ text e ])
                       )
                )

        Graphql.Http.HttpError _ ->
            paragraph []
                [ text <|
                    "Could not connect to the DE-STRESS server. Check your internet "
                        ++ "connection and try again. If your connection is fine "
                        ++ "then the DE-STRESS server might be unavailable."
                ]



-- }}}
-- {{{ Plotting


createAllHistogramsSpec : DesignMetrics -> List DesignMetrics -> VL.Spec
createAllHistogramsSpec designMetrics pdbMetricsList =
    case List.filter (\a -> a.hydrophobicFitness /= Nothing) pdbMetricsList of
        [] ->
            VL.toVegaLite []

        firstPdb :: remainingPdb ->
            let
                pdbData =
                    (List.map metricDataRow remainingPdb
                        |> List.foldl (>>) (metricDataRow firstPdb)
                    )
                        >> VL.dataFromRows []

                designData =
                    metricDataRow designMetrics
                        >> VL.dataFromRows []
            in
            [ VL.asSpec
                [ List.map
                    (histogramSpec (pdbData []) (designData []))
                    [ "Hydrophobic Fitness"
                    , "Isoelectric Point"
                    ]
                    |> VL.hConcat
                ]
            , VL.asSpec
                [ List.map
                    (histogramSpec (pdbData []) (designData []))
                    [ "# Residues"
                    , "Mass"
                    ]
                    |> VL.hConcat
                ]
            , VL.asSpec
                [ List.map
                    (histogramSpec (pdbData []) (designData []))
                    [ "Mean Packing Density"
                    ]
                    |> VL.hConcat
                ]
            ]
                |> VL.vConcat
                |> List.singleton
                |> VL.toVegaLite


metricDataRow : DesignMetrics -> (List VL.DataRow -> List VL.DataRow)
metricDataRow { hydrophobicFitness, isoelectricPoint, mass, numOfResidues, packingDensity } =
    VL.dataRow
        [ ( "Hydrophobic Fitness", VL.num <| Maybe.withDefault 666 hydrophobicFitness )
        , ( "Isoelectric Point", VL.num isoelectricPoint )
        , ( "Mass", VL.num mass )
        , ( "# Residues", VL.num <| toFloat numOfResidues )
        , ( "Mean Packing Density", VL.num packingDensity )
        ]


histogramSpec : VL.Data -> VL.Data -> String -> VL.Spec
histogramSpec pdbData designData fieldName =
    VL.asSpec
        [ VL.height 225
        , VL.width 225
        , VL.layer
            [ VL.asSpec
                [ pdbData
                , VL.bar []
                , (VL.encoding
                    << VL.position VL.X
                        [ VL.pName fieldName
                        , VL.pBin [ VL.biBase 10, VL.biDivide [ 4, 2 ] ]
                        , VL.pMType VL.Quantitative
                        , VL.pAxis [ VL.axTitle fieldName ]
                        ]
                    << VL.position VL.Y
                        [ VL.pAggregate VL.opCount
                        , VL.pMType VL.Quantitative
                        ]
                  )
                    []
                ]
            , VL.asSpec
                [ designData
                , VL.rule [ VL.maSize 3 ]
                , (VL.encoding
                    << VL.position VL.X
                        [ VL.pName fieldName
                        , VL.pMType VL.Quantitative
                        , VL.pAxis [ VL.axTitle "" ]
                        ]
                  )
                    []
                ]
            ]
        ]


compositionDictWithDefaultValues : Dict String Float -> Dict String Float
compositionDictWithDefaultValues inputDict =
    let
        expectedKeys =
            [ "A"
            , "C"
            , "D"
            , "E"
            , "F"
            , "G"
            , "H"
            , "I"
            , "K"
            , "L"
            , "M"
            , "N"
            , "P"
            , "Q"
            , "R"
            , "S"
            , "T"
            , "V"
            , "W"
            , "X"
            , "Y"
            ]

        getWithDefault dict k =
            Dict.get k dict
                |> Maybe.withDefault 0
                |> Tuple.pair k
    in
    List.map (getWithDefault inputDict) expectedKeys
        |> Dict.fromList


createCompositionSpec :
    DesignMetrics
    -> List RefSetMetrics
    -> VL.Spec
createCompositionSpec designMetrics referenceSetMetricsList =
    let
        numberInReferenceSet =
            List.length referenceSetMetricsList
                |> toFloat

        designComposition =
            designMetrics.composition
                |> compositionDictWithDefaultValues

        referenceSetComposition =
            List.map
                .composition
                referenceSetMetricsList
                |> List.foldl
                    (\dictA dictB ->
                        Dict.merge
                            (\key a -> Dict.insert key a)
                            (\key a b -> Dict.insert key (a + b))
                            (\key b -> Dict.insert key b)
                            dictA
                            dictB
                            Dict.empty
                    )
                    Dict.empty
                |> Dict.toList
                |> List.map
                    (Tuple.mapSecond
                        (\v ->
                            v / numberInReferenceSet
                        )
                    )
                |> Dict.fromList
                |> compositionDictWithDefaultValues
    in
    compositionSpec designComposition referenceSetComposition


compositionSpec : Dict String Float -> Dict String Float -> VL.Spec
compositionSpec designCompositionDict pdbCompositionDict =
    let
        data =
            VL.dataFromColumns []
                << VL.dataColumn
                    "Amino Acids"
                    (VL.strs <|
                        List.concat
                            [ Dict.keys designCompositionDict
                            , Dict.keys pdbCompositionDict
                            ]
                    )
                << VL.dataColumn
                    "Proportion"
                    (VL.nums <|
                        List.concat
                            [ Dict.values designCompositionDict
                            , Dict.values pdbCompositionDict
                            ]
                    )
                << VL.dataColumn
                    "Set"
                    (VL.strs <|
                        List.concat
                            [ List.repeat (List.length <| Dict.keys designCompositionDict) "Design"
                            , List.repeat (List.length <| Dict.keys pdbCompositionDict)
                                "Reference"
                            ]
                    )

        config =
            (VL.configure
                << VL.configuration (VL.coView [ VL.vicoStroke <| Just "transparent" ])
                << VL.configuration (VL.coAxis [ VL.axcoDomainWidth 1 ])
            )
                []
    in
    VL.toVegaLite
        [ data []
        , VL.spacing 2
        , VL.bar
            []
        , (VL.encoding
            << VL.column
                [ VL.fName "Amino Acids"
                , VL.fMType VL.Nominal
                ]
            << VL.position VL.Y
                [ VL.pName "Proportion"
                , VL.pAggregate VL.opSum
                , VL.pMType VL.Quantitative
                , VL.pAxis [ VL.axTitle "Amino Acid Proportion", VL.axGrid True ]
                ]
            << VL.position VL.X
                [ VL.pName "Set"
                , VL.pMType VL.Nominal
                , VL.pAxis [ VL.axTitle "" ]
                , VL.pScale [ VL.scRangeStep <| Just 12 ]
                ]
            << VL.color [ VL.mName "Set", VL.mMType VL.Nominal, VL.mLegend [] ]
          )
            []
        , config
        ]


type alias TorsionAnglesDict =
    Dict String ( Float, Float, Float )


createTorsionAngleSpec : DesignMetrics -> List RefSetMetrics -> VL.Spec
createTorsionAngleSpec designMetrics referenceSetMetricsList =
    torsionAnglesSpec
        designMetrics.torsionAngles
        (List.map .torsionAngles referenceSetMetricsList)


torsionAnglesSpec : TorsionAnglesDict -> List TorsionAnglesDict -> VL.Spec
torsionAnglesSpec designTorsionAngles pdbTorsionAnglesDicts =
    let
        designIds =
            Dict.keys designTorsionAngles

        ( designOms, designPhis, designPsis ) =
            Dict.values designTorsionAngles
                |> ListExtra.unzipTriples

        designData =
            VL.dataFromColumns []
                << VL.dataColumn
                    "Omega"
                    (VL.nums designOms)
                << VL.dataColumn
                    "Phi"
                    (VL.nums designPhis)
                << VL.dataColumn
                    "Psi"
                    (VL.nums designPsis)
                << VL.dataColumn
                    "Residue ID"
                    (VL.strs designIds)

        ( pdbOms, pdbPhis, pdbPsis ) =
            List.map Dict.values pdbTorsionAnglesDicts
                |> List.concat
                |> ListExtra.unzipTriples

        pdbData =
            VL.dataFromColumns []
                << VL.dataColumn
                    "Omega"
                    (VL.nums pdbOms)
                << VL.dataColumn
                    "Phi"
                    (VL.nums pdbPhis)
                << VL.dataColumn
                    "Psi"
                    (VL.nums pdbPsis)

        sel =
            VL.selection
                << VL.select "grid"
                    VL.seInterval
                    [ VL.seBindScales
                    , VL.seZoom
                        "wheel![event.shiftKey]"
                    ]
    in
    VL.toVegaLite
        [ VL.vConcat
            [ VL.asSpec
                [ VL.height 400
                , VL.width 400
                , VL.layer
                    [ VL.asSpec
                        [ pdbData []
                        , VL.rect []
                        , (VL.encoding
                            << VL.position VL.X
                                [ VL.pName "Phi"
                                , VL.pBin [ VL.biStep 2 ]
                                , VL.pMType VL.Quantitative
                                , VL.pAxis [ VL.axTitle "Phi" ]
                                ]
                            << VL.position VL.Y
                                [ VL.pName "Psi"
                                , VL.pBin [ VL.biStep 2 ]
                                , VL.pMType VL.Quantitative
                                , VL.pAxis [ VL.axTitle "Psi" ]
                                ]
                            << VL.color
                                [ VL.mAggregate VL.opCount
                                , VL.mMType VL.Quantitative
                                , VL.mLegend [ VL.leTitle "PDB Counts" ]
                                ]
                          )
                            []
                        ]
                    , VL.asSpec
                        [ designData []
                        , sel []
                        , VL.point
                            [ VL.maTooltip VL.ttData ]
                        , (VL.encoding
                            << VL.position VL.X
                                [ VL.pName "Phi"
                                , VL.pMType VL.Quantitative
                                ]
                            << VL.position VL.Y
                                [ VL.pName "Psi"
                                , VL.pMType VL.Quantitative
                                ]
                            << VL.color [ VL.mStr "#666" ]
                          )
                            []
                        ]
                    ]
                ]
            , VL.asSpec
                [ VL.width 400
                , designData []
                , sel []
                , VL.tick
                    [ VL.maTooltip VL.ttData ]
                , (VL.encoding
                    << VL.position VL.X
                        [ VL.pName "Omega"
                        , VL.pMType VL.Quantitative
                        , VL.pAxis [ VL.axTitle "Design Omega Values" ]
                        ]
                  )
                    []
                ]
            , VL.asSpec
                [ VL.width 400
                , pdbData []
                , sel []
                , VL.tick []
                , (VL.encoding
                    << VL.position VL.X
                        [ VL.pName "Omega"
                        , VL.pMType VL.Quantitative
                        , VL.pAxis [ VL.axTitle "PDB Omega Values" ]
                        ]
                  )
                    []
                ]
            ]
        ]



-- }}}
