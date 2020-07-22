module Pages.Designs exposing (Model, Msg, Params, page)

import Biomolecules
import Codec exposing (Value)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Keyed as Keyed
import FeatherIcons
import File exposing (File)
import File.Select as FileSelect
import Html
import Html.Attributes as HAtt
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
    { mResourceUuid : Maybe ResourceUuid
    , loadingState : DesignLoadingState
    , loadErrors : List String
    , designs : Dict String Design.StoredDesign

    -- , mSelectedSpecification : Maybe Specification
    -- , overviewOptionDropDown : DropDown.Model String
    -- , mOverviewInfo : Maybe MetricPlots.ColumnData
    , deleteAllStatus : Buttons.DangerStatus
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
    in
    ( { mResourceUuid = mResourceUuid
      , loadingState = Free
      , loadErrors = []
      , designs = designDict

      --, mSelectedSpecification = Nothing
      --, overviewOptionDropDown = DropDown.init <| Tuple.first defaultPlotableOption
      --, mOverviewInfo = Nothing
      , deleteAllStatus = Buttons.initDangerStatus
      }
    , Cmd.none
      -- , Cmd.batch
      --     (case shared.appState of
      --         Shared.Running runState ->
      --             case runState.mSelectedSpecification of
      --                 Just uuidString ->
      --                     [ Codec.encoder Codec.string uuidString
      --                         |> Ports.getSpecificationForDesignsPage
      --                     ]
      --                 Nothing ->
      --                     []
      --         _ ->
      --             []
      --     )
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

                        design : Design
                        design =
                            { name =
                                String.split "." name
                                    |> List.head
                                    |> Maybe.withDefault name
                                    |> Editable.NotEditing
                            , fileName = name
                            , pdbString =
                                contents
                                    |> String.lines
                                    |> List.filter (String.startsWith "ATOM")
                                    |> String.join "\n"
                            , deleteStatus = Buttons.initDangerStatus

                            --, metricsJobStatus = Ports.Ready
                            , mMeetsActiveSpecification = Nothing
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
                    , Design.storeDesign
                        { uuidString = uuidString
                        , design = design |> Codec.encoder Design.codec
                        }
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
            ( model
              -- { model
              --     | mSelectedSpecification =
              --         Codec.decodeValue specWithUuidCodec specificationValue
              --             |> Result.toMaybe
              --             |> Maybe.map .specification
              --   }
            , Cmd.none
            )

        DeleteDesign uuidString dangerStatus ->
            if Buttons.isConfirmed dangerStatus then
                let
                    updatedDesigns =
                        Dict.remove uuidString model.designs
                in
                ( { model | designs = updatedDesigns }
                , Design.deleteDesign { uuidString = uuidString }
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
                ( { model | deleteAllStatus = Buttons.initDangerStatus }
                , Design.deleteAllDesigns ()
                )

            else
                ( { model | deleteAllStatus = dangerStatus }
                , Cmd.none
                )



-- Subscription Msgs
-- ( Global.Running runState, CheckForPlotUpdate newNumberOfMetrics _ ) ->
--     ( { model | previousNumberOfMetrics = newNumberOfMetrics }
--     , if newNumberOfMetrics /= model.previousNumberOfMetrics then
--         Ports.vegaPlot <|
--             { plotId = "overview"
--             , spec =
--                 Metrics.overviewSpec
--                     "Hydrophobic Fitness"
--                     (runState.designs
--                         |> Dict.toList
--                         |> List.map
--                             (\( k, v ) ->
--                                 ( k, Global.storedDesignToStub v )
--                             )
--                         |> List.map
--                             (createDesignCardData
--                                 (runState.mSelectedReferenceSet
--                                     |> Maybe.andThen
--                                         (\k -> Dict.get k runState.referenceSets)
--                                     |> Maybe.map
--                                         (Global.storedReferenceSetToStub
--                                             >> ReferenceSet.getParamsForStub
--                                             >> .aggregateData
--                                         )
--                                 )
--                                 model.mSelectedSpecification
--                             )
--                         |> List.indexedMap Tuple.pair
--                         |> List.reverse
--                         |> List.filterMap
--                             (makeColumnData <|
--                                 .hydrophobicFitness
--                                     >> Maybe.withDefault (0 / 0)
--                                     >> abs
--                             )
--                         |> List.sortBy .value
--                         |> List.reverse
--                         |> List.map (\{ name, value } -> ( name, value ))
--                         |> Dict.fromList
--                     )
--             }
--       else
--         Cmd.none
--     , Cmd.none
--     )
-- Drop Downs
-- OverviewOptionDropDownMsg cMsg ->
--     let
--         cModel =
--             DropDown.update cMsg model.overviewOptionDropDown
--     in
--     ( { model | overviewOptionDropDown = cModel }
--     , Cmd.none
--     , Cmd.none
--     )


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
        Just { resourceUuid, designs } ->
            ( { model
                | mResourceUuid = Just resourceUuid
                , designs = designs
              }
            , Cmd.none
            )

        Nothing ->
            Debug.todo "Should I deal with this or leave to the shared view?"


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- }}}
-- {{{ VIEW


view : Model -> Document Msg
view model =
    { title = "Designs"
    , body = [ bodyView model ]
    }


bodyView : Model -> Element Msg
bodyView { designs, loadingState, deleteAllStatus } =
    let
        designCardData =
            designs
                |> Dict.toList
                |> List.map
                    (\( k, v ) ->
                        ( k, Design.storedDesignToStub v )
                    )
                |> List.map
                    (createDesignCardData
                        Nothing
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
                     -- model.mSelectedSpecification
                    )
    in
    column [ spacing 15, width fill ]
        [ let
            ( buttonLabel, isActive ) =
                case loadingState of
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
                    , status = deleteAllStatus
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
                        -- model.mSelectedSpecification
                        Nothing
                        designCardData
                    ]
        ]


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
        Nothing

    -- case ( mSpecification, designStub.metricsJobStatus ) of
    --     ( Just specification, Ports.Complete designMetrics ) ->
    --         Specification.applySpecification mAggregateData
    --             designMetrics
    --             specification
    --             |> Just
    --     _ ->
    --         Nothing
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
            ]
            [ link Style.linkStyle
                { url =
                    Route.Designs__Uuid_String { uuid = uuidString }
                        |> Route.toString
                , label = Style.h2 <| text designStub.name
                }

            -- , case designStub.metricsJobStatus of
            --     Ports.Ready ->
            --         text "Ready for server submission."
            --     Ports.Submitted _ ->
            --         text "Job submitted to server."
            --     Ports.Queued ->
            --         text "Job queued on server."
            --     Ports.InProgress ->
            --         text "Job is running on server."
            --     Ports.Cancelled ->
            --         text "Job was cancelled by user."
            --     Ports.Failed errorString ->
            --         "Server error while creating metrics: "
            --             ++ errorString
            --             |> text
            --     Ports.Complete _ ->
            --         text "Metrics Available"
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
                [ Style.h2 <| text "Meets Specification"
                , meetsSpecification
                    |> List.map designCard
                    |> cardContainer
                , Style.h2 <| text "Failed to Meet Specification"
                , failedSpecification
                    |> List.map designCard
                    |> cardContainer
                , Style.h2 <| text "No Metrics Available"
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
    -> Maybe ColumnData
makeColumnData getDataFn ( index, { uuidString, designStub, mMeetsSpecification } ) =
    Nothing



-- case designStub.metricsJobStatus of
--     WebSockets.Complete metrics ->
--         Just
--             { index = toFloat index
--             , name = designStub.name
--             , uuidString = uuidString
--             , value = getDataFn metrics
--             , mMeetsSpecification = mMeetsSpecification
--             }
--     _ ->
--         Nothing
-- }}}
-- }}}
