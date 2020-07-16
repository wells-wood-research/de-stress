module Pages.Designs exposing (Model, Msg, page)

import Ampal
import Codec exposing (Value)
import Components.DropDown as DropDown
import Design
import Dict exposing (Dict)
import Editable exposing (Editable(..))
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Keyed as Keyed
import FeatherIcons
import File exposing (File)
import File.Select as FileSelect
import Generated.Params as Params
import Generated.Routes as Routes
import Global
import Html
import Html.Attributes as HAtt
import Metrics exposing (DesignMetrics)
import Metrics.Plots as MetricPlots
import Ports
import ReferenceSet
import Spa.Page exposing (send)
import Specification exposing (Specification)
import Style exposing (h1, h2)
import Task
import Time
import Utils.Spa exposing (Page)


page : Page Params.Designs Model Msg model msg appMsg
page =
    Spa.Page.component
        { title = always "Designs"
        , init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- {{{ Init


type alias Model =
    { loadingState : DesignLoadingState
    , loadErrors : List String
    , mSelectedSpecification : Maybe Specification
    , overviewOptionDropDown : DropDown.Model String
    , previousNumberOfMetrics : Int
    , mOverviewInfo : Maybe MetricPlots.ColumnData
    , deleteAllStatus : Style.DangerStatus
    }


type DesignLoadingState
    = LoadingFiles Int Int
    | Free


init : Utils.Spa.PageContext -> Params.Designs -> ( Model, Cmd Msg, Cmd Global.Msg )
init { global } _ =
    ( { loadingState = Free
      , loadErrors = []
      , mSelectedSpecification = Nothing
      , overviewOptionDropDown = DropDown.init <| Tuple.first defaultPlotableOption
      , previousNumberOfMetrics = 0
      , mOverviewInfo = Nothing
      , deleteAllStatus = Style.Unclicked
      }
    , Cmd.none
    , Cmd.batch
        (case global of
            Global.Running runState ->
                case runState.mSelectedSpecification of
                    Just uuidString ->
                        [ Codec.encoder Codec.string uuidString
                            |> Ports.getSpecificationForDesignsPage
                        ]

                    Nothing ->
                        []

            _ ->
                []
        )
    )



-- }}}
-- {{{ Update


type Msg
    = StructuresRequested
    | StructureFilesSelected File (List File)
    | StructureLoaded String String
    | GotSpecification Value
    | ShowDesignDetails String
    | DeleteDesign String Style.DangerStatus
    | DeleteAllDesigns Style.DangerStatus
      -- Subscription Msgs
    | CheckForPlotUpdate Int Time.Posix
      -- DropDowns
    | OverviewOptionDropDownMsg (DropDown.Msg String)


update : Utils.Spa.PageContext -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update { global } msg model =
    case ( global, msg ) of
        ( Global.FailedToLaunch _, _ ) ->
            Debug.todo "Deal with this"

        ( _, StructuresRequested ) ->
            ( model, structureRequested, Cmd.none )

        ( _, StructureFilesSelected first rest ) ->
            let
                loadedDesigns =
                    List.length rest
                        |> (+) 1
            in
            ( { model | loadingState = LoadingFiles loadedDesigns 0 }
            , Cmd.batch <|
                List.map
                    (\file ->
                        Task.perform (StructureLoaded <| File.name file)
                            (File.toString file)
                    )
                    (first :: rest)
            , Cmd.none
            )

        ( _, StructureLoaded name contents ) ->
            let
                loadingState =
                    case model.loadingState of
                        LoadingFiles total remaining ->
                            let
                                updatedRemaining =
                                    remaining + 1
                            in
                            if updatedRemaining == total then
                                Free

                            else
                                LoadingFiles total updatedRemaining

                        Free ->
                            Free

                rStructuralData =
                    Ampal.parsePdbString name contents
            in
            case rStructuralData of
                Ok _ ->
                    -- Currently this is only checking to see if the file is valid PDB
                    ( { model | loadingState = loadingState }
                    , Cmd.none
                    , { name =
                            String.split "." name
                                |> List.head
                                |> Maybe.withDefault name
                                |> NotEditing
                      , fileName = name
                      , pdbString =
                            contents
                                |> String.lines
                                |> List.filter (String.startsWith "ATOM")
                                |> String.join "\n"
                      , deleteStatus = Style.Unclicked
                      , metricsJobStatus = Ports.Ready
                      , mMeetsActiveSpecification = Nothing
                      }
                        |> Global.AddDesign
                        |> send
                    )

                Err (Ampal.PdbParseError errorString) ->
                    ( { model
                        | loadErrors =
                            ("Failed to parse PDB file "
                                ++ name
                                ++ ":\n\t"
                                ++ errorString
                            )
                                :: model.loadErrors
                        , loadingState = loadingState
                      }
                    , Cmd.none
                    , Cmd.none
                    )

                Err (Ampal.HttpError _) ->
                    ( { model
                        | loadErrors =
                            ("Something weird happened while loading "
                                ++ name
                            )
                                :: model.loadErrors
                        , loadingState = loadingState
                      }
                    , Cmd.none
                    , Cmd.none
                    )

        ( _, GotSpecification specificationValue ) ->
            let
                specWithUuidCodec =
                    Codec.object
                        (\uuidString specification ->
                            { uuidString = uuidString
                            , specification = specification
                            }
                        )
                        |> Codec.field "uuidString" .uuidString Codec.string
                        |> Codec.field "specification"
                            .specification
                            Specification.codec
                        |> Codec.buildObject
            in
            ( { model
                | mSelectedSpecification =
                    Codec.decodeValue specWithUuidCodec specificationValue
                        |> Result.toMaybe
                        |> Maybe.map .specification
              }
            , Cmd.none
            , Cmd.none
            )

        ( _, ShowDesignDetails uuidString ) ->
            ( model
            , Cmd.none
            , Routes.routes.designs_dynamic uuidString
                |> Global.NavigateTo
                |> send
            )

        ( _, DeleteDesign uuidString dangerStatus ) ->
            ( model
            , Cmd.none
            , Global.DeleteDesign uuidString dangerStatus
                |> send
            )

        ( _, DeleteAllDesigns dangerStatus ) ->
            case dangerStatus of
                Style.Confirmed ->
                    ( { model | deleteAllStatus = Style.Unclicked }
                    , Cmd.none
                    , Global.DeleteAllDesigns dangerStatus
                        |> send
                    )

                _ ->
                    ( { model | deleteAllStatus = dangerStatus }
                    , Cmd.none
                    , Cmd.none
                    )

        -- Subscription Msgs
        ( Global.Running runState, CheckForPlotUpdate newNumberOfMetrics _ ) ->
            ( { model | previousNumberOfMetrics = newNumberOfMetrics }
            , if newNumberOfMetrics /= model.previousNumberOfMetrics then
                Ports.vegaPlot <|
                    { plotId = "overview"
                    , spec =
                        Metrics.overviewSpec
                            "Hydrophobic Fitness"
                            (runState.designs
                                |> Dict.toList
                                |> List.map
                                    (\( k, v ) ->
                                        ( k, Global.storedDesignToStub v )
                                    )
                                |> List.map
                                    (createDesignCardData
                                        (runState.mSelectedReferenceSet
                                            |> Maybe.andThen
                                                (\k -> Dict.get k runState.referenceSets)
                                            |> Maybe.map
                                                (Global.storedReferenceSetToStub
                                                    >> ReferenceSet.getParamsForStub
                                                    >> .aggregateData
                                                )
                                        )
                                        model.mSelectedSpecification
                                    )
                                |> List.indexedMap Tuple.pair
                                |> List.reverse
                                |> List.filterMap
                                    (makeColumnData <|
                                        .hydrophobicFitness
                                            >> Maybe.withDefault (0 / 0)
                                            >> abs
                                    )
                                |> List.sortBy .value
                                |> List.reverse
                                |> List.map (\{ name, value } -> ( name, value ))
                                |> Dict.fromList
                            )
                    }

              else
                Cmd.none
            , Cmd.none
            )

        -- Drop Downs
        ( _, OverviewOptionDropDownMsg cMsg ) ->
            let
                cModel =
                    DropDown.update cMsg model.overviewOptionDropDown
            in
            ( { model | overviewOptionDropDown = cModel }
            , Cmd.none
            , Cmd.none
            )


structureRequested : Cmd Msg
structureRequested =
    FileSelect.files [ "*/*" ] StructureFilesSelected



-- }}}
-- {{{ Subscriptions


subscriptions : Utils.Spa.PageContext -> Model -> Sub Msg
subscriptions { global } _ =
    Sub.batch
        (Ports.specificationForDesignsPage GotSpecification
            :: (case global of
                    Global.Running runState ->
                        [ runState.designs
                            |> Dict.values
                            |> List.map Global.storedDesignToStub
                            |> List.map .metricsJobStatus
                            |> List.map Ports.metricsAvailable
                            |> List.filter identity
                            |> List.length
                            |> CheckForPlotUpdate
                            |> Time.every 1000
                        ]

                    Global.FailedToLaunch _ ->
                        []
               )
        )



-- }}}
-- {{{ View


view : Utils.Spa.PageContext -> Model -> Element Msg
view { global } model =
    case global of
        Global.Running { designs, mSelectedReferenceSet, referenceSets } ->
            let
                designCardData =
                    designs
                        |> Dict.toList
                        |> List.map
                            (\( k, v ) ->
                                ( k, Global.storedDesignToStub v )
                            )
                        |> List.map
                            (createDesignCardData
                                (mSelectedReferenceSet
                                    |> Maybe.andThen
                                        (\k -> Dict.get k referenceSets)
                                    |> Maybe.map
                                        (Global.storedReferenceSetToStub
                                            >> ReferenceSet.getParamsForStub
                                            >> .aggregateData
                                        )
                                )
                                model.mSelectedSpecification
                            )
            in
            column [ spacing 15, width fill ]
                [ let
                    ( buttonLabel, isActive ) =
                        case model.loadingState of
                            LoadingFiles total remaining ->
                                ( "Loaded "
                                    ++ String.fromInt remaining
                                    ++ "/"
                                    ++ String.fromInt total
                                , False
                                )

                            Free ->
                                ( "Load", True )
                  in
                  row [ centerX, spacing 10 ]
                    [ h1 <| text "Designs"
                    , Style.conditionalButton
                        { label = text buttonLabel
                        , clickMsg = Just StructuresRequested
                        , isActive = isActive
                        }
                    , Style.dangerousButton
                        { label = text "Delete All"
                        , confirmText = "Are you sure you want to delete ALL design?"
                        , status = model.deleteAllStatus
                        , dangerousMsg = DeleteAllDesigns
                        }
                    ]
                , el [ width fill ] <|
                    if List.isEmpty designCardData then
                        el [ centerX ] (text "Click \"Load\" to add models.")

                    else
                        column
                            [ width fill ]
                            [ overviewPlots

                            -- model.overviewOptionDropDown
                            -- designCardData
                            , designCardsView
                                model.mSelectedSpecification
                                designCardData
                            ]
                ]

        Global.FailedToLaunch _ ->
            Debug.todo "Add common state page"


type alias DesignCardData =
    { uuidString : String
    , designStub : Design.DesignStub
    , mMeetsSpecification :
        Maybe Bool
    }


createDesignCardData :
    Maybe Metrics.AggregateData
    -> Maybe Specification
    -> ( String, Design.DesignStub )
    -> DesignCardData
createDesignCardData mAggregateData mSpecification ( uuidString, designStub ) =
    { uuidString = uuidString
    , designStub = designStub
    , mMeetsSpecification =
        case ( mSpecification, designStub.metricsJobStatus ) of
            ( Just specification, Ports.Complete designMetrics ) ->
                Specification.applySpecification mAggregateData
                    designMetrics
                    specification
                    |> Just

            _ ->
                Nothing
    }


partitionDesignCardData :
    List DesignCardData
    ->
        { meetsSpecification : List DesignCardData
        , failedSpecification : List DesignCardData
        , noMetrics : List DesignCardData
        }
    ->
        { meetsSpecification : List DesignCardData
        , failedSpecification : List DesignCardData
        , noMetrics : List DesignCardData
        }
partitionDesignCardData remainingData partitionedData =
    case remainingData of
        [] ->
            partitionedData

        data :: rest ->
            let
                newPartitioned =
                    case data.mMeetsSpecification of
                        Just True ->
                            { partitionedData
                                | meetsSpecification =
                                    data :: partitionedData.meetsSpecification
                            }

                        Just False ->
                            { partitionedData
                                | failedSpecification =
                                    data :: partitionedData.failedSpecification
                            }

                        Nothing ->
                            { partitionedData
                                | noMetrics =
                                    data :: partitionedData.noMetrics
                            }
            in
            partitionDesignCardData rest newPartitioned


designCard :
    { uuidString : String
    , designStub : Design.DesignStub
    , mMeetsSpecification :
        Maybe Bool
    }
    -> Element Msg
designCard { uuidString, designStub, mMeetsSpecification } =
    row
        [ mouseOver [ Background.color Style.colorPalette.c4 ]
        , fillPortion 1 |> width
        , Background.color Style.colorPalette.c5
        , case mMeetsSpecification of
            Nothing ->
                Border.color Style.colorPalette.c5

            Just False ->
                Border.color Style.colorPalette.red

            Just True ->
                Border.color Style.colorPalette.c3
        , Border.width 4
        , Border.rounded 10
        ]
        [ column
            [ padding 10
            , width fill
            , Events.onMouseUp <|
                ShowDesignDetails uuidString
            ]
            [ Style.h2 <| text designStub.name
            , case designStub.metricsJobStatus of
                Ports.Ready ->
                    text "Ready for server submission."

                Ports.Submitted _ ->
                    text "Job submitted to server."

                Ports.Queued ->
                    text "Job queued on server."

                Ports.InProgress ->
                    text "Job is running on server."

                Ports.Cancelled ->
                    text "Job was cancelled by user."

                Ports.Failed errorString ->
                    "Server error while creating metrics: "
                        ++ errorString
                        |> text

                Ports.Complete _ ->
                    text "Metrics Available"
            ]
        , el [ alignRight, padding 10 ] <|
            Style.dangerousButton
                { label = Style.featherIconToElmUi FeatherIcons.trash2
                , confirmText = "Are you sure you want to delete this design?"
                , status = designStub.deleteStatus
                , dangerousMsg = DeleteDesign uuidString
                }
        ]


designCardsView : Maybe Specification -> List DesignCardData -> Element Msg
designCardsView mSelectedSpecification designCardData =
    let
        cardContainer =
            wrappedRow [ spacing 10, width fill ]
    in
    case mSelectedSpecification of
        Just _ ->
            let
                { meetsSpecification, failedSpecification, noMetrics } =
                    partitionDesignCardData designCardData
                        { meetsSpecification = []
                        , failedSpecification = []
                        , noMetrics = []
                        }
            in
            column [ spacing 15, width fill ]
                [ h2 <| text "Meets Specification"
                , meetsSpecification
                    |> List.map designCard
                    |> cardContainer
                , h2 <| text "Failed to Meet Specification"
                , failedSpecification
                    |> List.map designCard
                    |> cardContainer
                , h2 <| text "No Metrics Available"
                , noMetrics
                    |> List.map designCard
                    |> cardContainer
                ]

        Nothing ->
            designCardData
                |> List.map designCard
                |> cardContainer



-- {{{ Overview Plots


defaultPlotableOption : ( String, DesignMetrics -> Float )
defaultPlotableOption =
    ( "Packing Density", .packingDensity )


plotableMetrics : Dict String (DesignMetrics -> Float)
plotableMetrics =
    [ defaultPlotableOption
    , ( "Hydrophobic Fitness"
      , .hydrophobicFitness
            >> Maybe.withDefault (0 / 0)
            >> abs
      )
    , ( "Isoelectric Point", .isoelectricPoint )
    , ( "Mass", .mass )
    ]
        |> Dict.fromList


overviewPlots : Element msg
overviewPlots =
    column
        [ width fill ]
        [ Style.h3 <| text "Overview"
        , Keyed.el [ centerX, width fill ]
            ( "overview"
            , Html.div
                [ HAtt.id "overview"
                , HAtt.style "width" "100%"
                ]
                [ Html.div
                    [ HAtt.height 200
                    , HAtt.style "height" "200px"
                    , HAtt.style "width" "100%"
                    , HAtt.style "border-radius" "5px"
                    , HAtt.style "background-color" "#d3d3d3"
                    ]
                    []
                ]
                |> html
            )
        ]



-- overviewPlots :
--     DropDown.Model String
--     -> List DesignCardData
--     -> Element Msg
-- overviewPlots ({ selected } as dropDownModel) designCardData =
--     let
--         getDataFn =
--             Dict.get selected plotableMetrics
--                 |> Maybe.withDefault (Tuple.second defaultPlotableOption)
--     in
--     column [ spacing 10, fill |> maximum 500 |> height, width fill ]
--         [ Style.h2 <| text "Overview"
--         , el [ width <| maximum 300 <| fill ]
--             (DropDown.view text (Dict.keys plotableMetrics) dropDownModel
--                 |> map OverviewOptionDropDownMsg
--             )
--         , List.indexedMap Tuple.pair designCardData
--             |> List.reverse
--             |> List.filterMap (makeColumnData getDataFn)
--             |> List.sortBy .value
--             |> List.reverse
--             |> MetricPlots.metricOverview
--                 ShowDesignDetails
--                 selected
--             |> html
--         ]


makeColumnData :
    (DesignMetrics -> Float)
    -> ( Int, DesignCardData )
    -> Maybe MetricPlots.ColumnData
makeColumnData getDataFn ( index, { uuidString, designStub, mMeetsSpecification } ) =
    case designStub.metricsJobStatus of
        Ports.Complete metrics ->
            Just
                { index = toFloat index
                , name = designStub.name
                , uuidString = uuidString
                , value = getDataFn metrics
                , mMeetsSpecification = mMeetsSpecification
                }

        _ ->
            Nothing



-- }}}
-- }}}
