port module Pages.Designs exposing (Model, Msg, Params, page)

import Biomolecules
import Browser.Navigation exposing (Key)
import Codec exposing (Value)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Input as Input
import Element.Keyed as Keyed
import FeatherIcons
import File exposing (File)
import File.Select as FileSelect
import Html
import Html.Attributes as HAtt
import Set
import Shared
import Shared.Buttons as Buttons
import Shared.Design as Design exposing (Design)
import Shared.Editable as Editable
import Shared.Metrics as Metrics exposing (DesignMetrics)
import Shared.Plots as Plots exposing (ColumnData)
import Shared.ResourceUuid as ResourceUuid exposing (ResourceUuid)
import Shared.Specification as Specification exposing (Specification)
import Shared.Style as Style
import Shared.WebSockets as WebSockets
import Spa.Document exposing (Document)
import Spa.Generated.Route as Route
import Spa.Page as Page exposing (Page)
import Spa.Url as Url exposing (Url)
import Task
import Utils.Route exposing (navigate)
import VegaLite


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



-- {{{ PORTS


port setSpecificationForDesign : (Value -> msg) -> Sub msg



-- }}}
-- {{{ MODEL


type alias Model =
    { mResourceUuid : Maybe ResourceUuid
    , loadingState : DesignLoadingState
    , loadErrors : List String
    , designs : Dict String Design.StoredDesign
    , mSelectedSpecification : Maybe Specification

    -- , overviewOptionDropDown : DropDown.Model String
    -- , mOverviewInfo : Maybe MetricPlots.ColumnData
    , deleteAllStatus : Buttons.DangerStatus
    , navKey : Key
    , selectedUuids : Set.Set String
    }


type DesignLoadingState
    = LoadingFiles Int Int
    | Free



-- }}}
-- {{{ INIT


type alias Params =
    ()


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared _ =
    let
        ( mResourceUuid, designDict ) =
            case Shared.getRunState shared of
                Just { resourceUuid, designs } ->
                    ( Just resourceUuid, designs )

                Nothing ->
                    ( Nothing, Dict.empty )

        model =
            { mResourceUuid = mResourceUuid
            , loadingState = Free
            , loadErrors = []
            , designs = designDict
            , mSelectedSpecification = Nothing

            --, overviewOptionDropDown = DropDown.init <| Tuple.first defaultPlotableOption
            --, mOverviewInfo = Nothing
            , deleteAllStatus = Buttons.initDangerStatus
            , navKey = shared.key
            , selectedUuids = Set.empty
            }
    in
    ( model
    , Cmd.batch
        [ case Shared.getRunState shared of
            Just runState ->
                case runState.mSelectedSpecification of
                    Just uuidString ->
                        Specification.getSpecificationForDesignsPage
                            { uuidString = uuidString }

                    Nothing ->
                        Cmd.none

            Nothing ->
                Cmd.none
        , makeOverViewSpecCmd model
        ]
    )



-- }}}
-- {{{ UPDATE


type Msg
    = StructuresRequested
    | StructureFilesSelected File (List File)
    | StructureLoaded String String
    | GotSpecification Value
    | DeleteDesign String Buttons.DangerStatus
    | DeleteAllDesigns Buttons.DangerStatus
    | DesignDetails String
    | SelectedDesign String Bool



-- DropDowns
-- | OverviewOptionDropDownMsg (DropDown.Msg String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StructuresRequested ->
            ( model, structureRequested )

        StructureFilesSelected first rest ->
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
            )

        StructureLoaded name contents ->
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
                    Biomolecules.parsePdbString name contents
            in
            case ( model.mResourceUuid, rStructuralData ) of
                ( Nothing, _ ) ->
                    Debug.todo "Add error panel"

                ( _, Err (Biomolecules.PdbParseError errorString) ) ->
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
                    )

                ( _, Err (Biomolecules.HttpError _) ) ->
                    ( { model
                        | loadErrors =
                            ("Something weird happened while loading "
                                ++ name
                            )
                                :: model.loadErrors
                        , loadingState = loadingState
                      }
                    , Cmd.none
                    )

                ( Just resourceUuid, Ok _ ) ->
                    -- Currently this is only checking to see if the file is valid PDB
                    let
                        { uuidString, nextResourceUuid } =
                            ResourceUuid.toString
                                resourceUuid

                        pdbString =
                            contents
                                |> String.lines
                                |> List.filter (String.startsWith "ATOM")
                                |> String.join "\n"

                        ( jobStatus, requestMetricsCmd ) =
                            WebSockets.prepareMetricsJob
                                { uuidString = uuidString, pdbString = pdbString }

                        design : Design
                        design =
                            { name =
                                String.split "." name
                                    |> List.head
                                    |> Maybe.withDefault name
                                    |> Editable.NotEditing
                            , fileName = name
                            , pdbString = pdbString
                            , deleteStatus = Buttons.initDangerStatus
                            , metricsJobStatus = jobStatus
                            , mMeetsActiveSpecification = Nothing
                            , tags = Set.empty
                            }
                    in
                    ( { model
                        | mResourceUuid = Just nextResourceUuid
                        , loadingState = loadingState
                        , designs =
                            Dict.insert uuidString
                                (Design.createDesignStub design
                                    |> Design.storeDesignStubLocally
                                )
                                model.designs
                      }
                    , Cmd.batch
                        [ Design.storeDesign
                            { uuidString = uuidString
                            , design = design |> Codec.encoder Design.codec
                            }
                        , requestMetricsCmd
                        ]
                    )

        GotSpecification specificationValue ->
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
            )

        DeleteDesign uuidString dangerStatus ->
            if Buttons.isConfirmed dangerStatus then
                let
                    updatedModel =
                        { model | designs = Dict.remove uuidString model.designs }
                in
                ( updatedModel
                , Cmd.batch
                    [ Design.deleteDesign { uuidString = uuidString }
                    , makeOverViewSpecCmd model
                    ]
                )

            else
                let
                    updatedDesigns =
                        Dict.update
                            uuidString
                            ((\des ->
                                { des | deleteStatus = dangerStatus }
                             )
                                |> Design.mapStoredDesign
                                |> Maybe.map
                            )
                            model.designs
                in
                ( { model | designs = updatedDesigns }
                , Cmd.none
                )

        DeleteAllDesigns dangerStatus ->
            if Buttons.isConfirmed dangerStatus then
                let
                    updatedModel =
                        { model
                            | deleteAllStatus = Buttons.initDangerStatus
                            , designs = Dict.empty
                        }
                in
                ( updatedModel
                , Cmd.batch
                    [ Design.deleteAllDesigns ()
                    , makeOverViewSpecCmd updatedModel
                    ]
                )

            else
                ( { model | deleteAllStatus = dangerStatus }
                , Cmd.none
                )

        DesignDetails uuid ->
            ( model
            , navigate
                model.navKey
                (Route.Designs__Uuid_String { uuid = uuid })
            )

        SelectedDesign uuid selected ->
            ( if selected then
                { model | selectedUuids = Set.insert uuid model.selectedUuids }

              else
                { model | selectedUuids = Set.remove uuid model.selectedUuids }
            , Cmd.none
            )


structureRequested : Cmd Msg
structureRequested =
    FileSelect.files [ "*/*" ] StructureFilesSelected


save : Model -> Shared.Model -> Shared.Model
save model shared =
    case model.mResourceUuid of
        Just resourceUuid ->
            Shared.mapRunState
                (\runState ->
                    { runState
                        | resourceUuid = resourceUuid
                        , designs = model.designs
                        , saveStateRequested = True
                    }
                )
                shared

        Nothing ->
            shared


load : Shared.Model -> Model -> ( Model, Cmd Msg )
load shared model =
    case Shared.getRunState shared of
        Just runState ->
            let
                updatedModel =
                    { model
                        | mResourceUuid = Just runState.resourceUuid
                        , designs = runState.designs
                    }
            in
            ( updatedModel
            , makeOverViewSpecCmd updatedModel
            )

        Nothing ->
            Debug.todo "Should I deal with this or leave to the shared view?"


makeOverViewSpecCmd : Model -> Cmd Msg
makeOverViewSpecCmd model =
    if Dict.isEmpty model.designs then
        Cmd.none

    else
        { plotId = "overview"
        , spec =
            Metrics.overviewSpec
                "Hydrophobic Fitness"
                (model.designs
                    |> Dict.toList
                    |> List.map
                        (\( k, v ) ->
                            ( k, Design.storedDesignToStub v )
                        )
                    |> List.map
                        (createDesignCardData
                            Nothing
                            -- (runState.mSelectedReferenceSet
                            --     |> Maybe.andThen
                            --         (\k -> Dict.get k runState.referenceSets)
                            --     |> Maybe.map
                            --         (ReferenceSet.storedReferenceSetToStub
                            --             >> ReferenceSet.getParamsForStub
                            --             >> .aggregateData
                            --         )
                            -- )
                            model.mSelectedSpecification
                            model.selectedUuids
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
            |> Plots.vegaPlot


subscriptions : Model -> Sub Msg
subscriptions _ =
    setSpecificationForDesign GotSpecification



-- }}}
-- {{{ VIEW


view : Model -> Document Msg
view model =
    { title = "Designs"
    , body = [ bodyView model ]
    }


bodyView : Model -> Element Msg
bodyView model =
    let
        designCardData =
            model.designs
                |> Dict.toList
                |> List.map
                    (\( k, v ) ->
                        ( k, Design.storedDesignToStub v )
                    )
                |> List.map
                    (createDesignCardData
                        Nothing
                        -- (mSelectedReferenceSet
                        --     |> Maybe.andThen
                        --         (\k -> Dict.get k referenceSets)
                        --     |> Maybe.map
                        --         (Global.storedReferenceSetToStub
                        --             >> ReferenceSet.getParamsForStub
                        --             >> .aggregateData
                        --         )
                        -- )
                        model.mSelectedSpecification
                        model.selectedUuids
                    )
    in
    el [ centerX, width <| maximum 800 <| fill ] <|
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
              wrappedRow [ centerX, spacing 10 ]
                [ paragraph [] [ Style.h1 <| text "Designs" ]
                , row [ spacing 10 ]
                    [ Buttons.conditionalButton
                        { label = text buttonLabel
                        , clickMsg = Just StructuresRequested
                        , isActive = isActive
                        }
                    , Buttons.dangerousButton
                        { label = text "Delete All"
                        , confirmText = "Are you sure you want to delete ALL design?"
                        , status = model.deleteAllStatus
                        , dangerousMsg = DeleteAllDesigns
                        }
                    ]
                ]
            , el [ width fill ] <|
                if List.isEmpty designCardData then
                    el [ centerX ] (text "Click \"Load\" to add models.")

                else
                    column
                        [ spacing 10, width fill ]
                        [ overviewPlots

                        -- model.overviewOptionDropDown
                        -- designCardData
                        , designCardsView
                            model.mSelectedSpecification
                            designCardData
                        ]
            ]


type alias DesignCardData =
    { uuidString : String
    , designStub : Design.DesignStub
    , mMeetsSpecification :
        Maybe Bool
    , selected : Bool
    }


createDesignCardData :
    Maybe Metrics.AggregateData
    -> Maybe Specification
    -> Set.Set String
    -> ( String, Design.DesignStub )
    -> DesignCardData
createDesignCardData mAggregateData mSpecification selectedUuids ( uuidString, designStub ) =
    { uuidString = uuidString
    , designStub = designStub
    , mMeetsSpecification =
        case ( mSpecification, WebSockets.getDesignMetrics designStub.metricsJobStatus ) of
            ( Just specification, Just designMetrics ) ->
                Specification.applySpecification mAggregateData
                    designMetrics
                    specification
                    |> Just

            _ ->
                Nothing
    , selected = Set.member uuidString selectedUuids
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


designCardView :
    { uuidString : String
    , designStub : Design.DesignStub
    , mMeetsSpecification :
        Maybe Bool
    , selected : Bool
    }
    -> Element Msg
designCardView { uuidString, designStub, mMeetsSpecification, selected } =
    row
        [ mouseOver [ Background.color Style.colorPalette.c4 ]
        , spacing 10
        , width fill
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
        [ Input.checkbox [ padding 5, width <| px 10 ]
            { onChange = SelectedDesign uuidString

            -- TODO Change this
            , icon = Input.defaultCheckbox
            , checked = selected
            , label = Input.labelAbove [] <| none
            }
        , column
            [ padding 10
            , pointer
            , width fill
            , DesignDetails uuidString
                |> Events.onClick
            , alignLeft
            ]
            [ Style.h2 <| text designStub.name
            , paragraph []
                [ WebSockets.metricsJobStatusString designStub.metricsJobStatus
                    |> text
                ]
            ]
        , el [ alignRight, padding 10 ] <|
            Buttons.dangerousButton
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
            column [ spacing 10, width fill ]
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
                [ Style.h2 <| text "Meets Specification"
                , meetsSpecification
                    |> List.map designCardView
                    |> cardContainer
                , Style.h2 <| text "Failed to Meet Specification"
                , failedSpecification
                    |> List.map designCardView
                    |> cardContainer
                , Style.h2 <| text "No Metrics Available"
                , noMetrics
                    |> List.map designCardView
                    |> cardContainer
                ]

        Nothing ->
            designCardData
                |> List.map designCardView
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
        , Keyed.el [ centerX ]
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
    -> Maybe ColumnData
makeColumnData getDataFn ( index, { uuidString, designStub, mMeetsSpecification } ) =
    WebSockets.getDesignMetrics designStub.metricsJobStatus
        |> Maybe.map
            (\metrics ->
                { index = toFloat index
                , name = designStub.name
                , uuidString = uuidString
                , value = getDataFn metrics
                , mMeetsSpecification = mMeetsSpecification
                }
            )



-- }}}
-- }}}
