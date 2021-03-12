module Pages.Designs.Overview exposing (Model, Msg, Params, page)

import Dict exposing (Dict)
import Element exposing (..)
import Element.Keyed as Keyed
import Html
import Html.Attributes as HAtt
import Shared
import Shared.Design as Design exposing (DesignStub)
import Shared.Metrics exposing (DesignMetrics)
import Shared.Plots as Plots
import Shared.Style as Style
import Shared.WebSockets as WebSockets
import Spa.Document exposing (Document)
import Spa.Page as Page exposing (Page)
import Spa.Url exposing (Url)
import VegaLite as VL


page : Page Params Model Msg
page =
    Page.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , save = save
        , load = load
        }



-- {{{ MODEL


type alias Model =
    { designStubs : Dict String DesignStub }



-- }}}
-- {{{ INIT


type alias Params =
    ()


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared _ =
    let
        model =
            { designStubs =
                case Shared.getRunState shared of
                    Just runState ->
                        runState.designs
                            |> Dict.toList
                            |> List.map (Tuple.mapSecond Design.storedDesignToStub)
                            |> Dict.fromList

                    Nothing ->
                        Dict.empty
            }
    in
    ( model
    , model.designStubs
        |> makeOverViewSpec
        |> Plots.vegaPlot
    )



-- }}}
-- {{{ UPDATE


type Msg
    = ReplaceMe


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReplaceMe ->
            ( model, Cmd.none )


save : Model -> Shared.Model -> Shared.Model
save _ shared =
    shared


load : Shared.Model -> Model -> ( Model, Cmd Msg )
load shared model =
    let
        updatedModel =
            { model
                | designStubs =
                    case Shared.getRunState shared of
                        Just runState ->
                            runState.designs
                                |> Dict.toList
                                |> List.map (Tuple.mapSecond Design.storedDesignToStub)
                                |> Dict.fromList

                        Nothing ->
                            Dict.empty
            }
    in
    ( updatedModel
    , updatedModel.designStubs
        |> makeOverViewSpec
        |> Plots.vegaPlot
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- {{{ VIEW


view : Model -> Document Msg
view model =
    { title = "Designs Overview"
    , body = [ bodyView model ]
    }


bodyView : Model -> Element msg
bodyView _ =
    el [ centerX, width <| maximum 800 <| fill ] <|
        column
            [ spacing 10, width fill ]
            [ el [ centerX ] <| Style.h1 <| text "All Designs Overview"
            , Style.h2 <| text "Metrics Plots"
            , paragraph []
                [ text "(Click bars to go to design page)"
                ]
            , Keyed.el [ centerX ]
                ( "overview"
                , Html.div
                    [ HAtt.id "overview"
                    , HAtt.style "width" "100%"
                    ]
                    [ Html.div
                        [ HAtt.style "height" "200px"
                        , HAtt.style "width" "100%"
                        , HAtt.style "border-radius" "5px"
                        , HAtt.style "background-color" "#d3d3d3"
                        ]
                        []
                    ]
                    |> html
                )
            ]


makeOverViewSpec : Dict String DesignStub -> { plotId : String, spec : VL.Spec }
makeOverViewSpec designStubs =
    { plotId = "overview"
    , spec =
        overviewSpec
            (designStubs
                |> Dict.toList
                |> List.filterMap makePlotData
            )
    }


makePlotData : ( String, DesignStub ) -> Maybe PlotData
makePlotData ( uuid, { name, metricsJobStatus } ) =
    WebSockets.getDesignMetrics metricsJobStatus
        |> Maybe.map
            (\metrics ->
                { uuid = uuid
                , name = name
                , hydrophobicFitness = Maybe.withDefault (0 / 0) metrics.hydrophobicFitness
                , isoelectricPoint = metrics.isoelectricPoint
                , numberOfResidues = metrics.numOfResidues |> toFloat
                , packingDensity = metrics.packingDensity
                , budeFFTotalEnergy =
                    Maybe.withDefault (0 / 0)
                        metrics.budeFFResults.totalEnergy
                , evoEFTotalEnergy =
                    Maybe.withDefault (0 / 0)
                        metrics.evoEF2Results.total
                , dfireTotalEnergy =
                    Maybe.withDefault (0 / 0)
                        metrics.dfire2Results.total
                , rosettaTotalEnergy =
                    Maybe.withDefault (0 / 0)
                        metrics.rosettaResults.total_score
                }
            )


type alias PlotData =
    { uuid : String
    , name : String
    , hydrophobicFitness : Float
    , isoelectricPoint : Float
    , numberOfResidues : Float
    , packingDensity : Float
    , budeFFTotalEnergy : Float
    , evoEFTotalEnergy : Float
    , dfireTotalEnergy : Float
    , rosettaTotalEnergy : Float
    }


plotTuples : List ( String, PlotData -> Float, VL.SortProperty )
plotTuples =
    [ ( "Hydrophobic Fitness", .hydrophobicFitness, VL.soAscending )
    , ( "Isoelectric Point", .isoelectricPoint, VL.soDescending )
    , ( "Number of Residues", .numberOfResidues, VL.soDescending )
    , ( "Packing Density", .packingDensity, VL.soDescending )
    , ( "BUDE FF Total Energy", .budeFFTotalEnergy, VL.soAscending )
    , ( "EvoEF2 Total Energy", .evoEFTotalEnergy, VL.soAscending )
    , ( "dFire Total Energy", .dfireTotalEnergy, VL.soAscending )
    , ( "Rosetta Total Energy", .rosettaTotalEnergy, VL.soAscending )
    ]


overviewSpec : List PlotData -> VL.Spec
overviewSpec plotData =
    let
        metricValueColumn ( label, valueFn, _ ) =
            VL.dataColumn
                label
                (VL.nums <|
                    List.map valueFn plotData
                )

        metricValueBarSpec ( label, _, sortProp ) =
            VL.asSpec
                [ VL.bar []
                , VL.title (label ++ "\nfor All Designs") []
                , VL.width 200
                , (VL.encoding
                    << VL.position VL.Y
                        [ VL.pName "Design Name"
                        , VL.pNominal
                        , VL.pSort
                            [ VL.soByField label VL.opMax
                            , sortProp
                            ]
                        , VL.pAxis
                            [ VL.axLabelExpr
                                "split(datum.label, '@@@')[0]"
                            ]
                        ]
                    << VL.position VL.X
                        [ VL.pName label
                        , VL.pMType VL.Quantitative
                        , VL.pAxis [ VL.axTitle label, VL.axGrid True ]
                        ]
                    << VL.hyperlink
                        [ VL.hName "url"
                        , VL.hNominal
                        ]
                  )
                    []
                ]

        dataColumns =
            List.map metricValueColumn plotTuples
                |> List.foldr (<<) identity

        data =
            VL.dataFromColumns []
                << VL.dataColumn
                    "Design Name"
                    (VL.strs <|
                        List.map (\{ name, uuid } -> name ++ "@@@" ++ uuid) plotData
                    )
                << VL.dataColumn
                    "uuid"
                    (VL.strs <|
                        List.map .uuid plotData
                    )
                << dataColumns

        config =
            (VL.configure
                << VL.configuration (VL.coView [ VL.vicoStroke <| Just "transparent" ])
                << VL.configuration (VL.coAxis [ VL.axcoDomainWidth 1 ])
            )
                []

        transform =
            VL.transform
                << VL.calculateAs "'/designs/' + datum.uuid" "url"
                << VL.calculateAs "datum.name + datum.uuid" "unique name"
    in
    VL.toVegaLite
        [ data []
        , VL.spacing 40
        , transform []
        , VL.vConcat <|
            List.map metricValueBarSpec plotTuples
        , config
        ]



-- }}}
