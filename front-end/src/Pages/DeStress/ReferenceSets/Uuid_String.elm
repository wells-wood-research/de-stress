port module Pages.DeStress.ReferenceSets.Uuid_String exposing (Model, Msg, Params, page)

import Browser.Navigation as Nav
import Codec exposing (Value)
import Dict
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Element.Keyed as Keyed
import Html
import Html.Attributes as HAtt
import Round
import Shared
import Shared.Buttons as Buttons
import Shared.Folds as Folds
import Shared.Metrics as Metrics exposing (RefSetMetrics)
import Shared.Plots as Plots
import Shared.ReferenceSet as ReferenceSet exposing (ReferenceSet, ReferenceSetStub)
import Shared.Style as Style
import Spa.Document exposing (Document)
import Spa.Generated.Route as Route
import Spa.Page as Page exposing (Page)
import Spa.Url exposing (Url)
import Utils.Route exposing (navigate)


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


port setFocussedReferenceSet :
    ({ uuidString : String, refSetValue : Value } -> msg)
    -> Sub msg



-- }}}
-- {{{ MODEL


type alias Model =
    { key : Nav.Key
    , pageState : PageState
    , displaySettings : DisplaySettings
    , device : Device
    }


type PageState
    = FailedToStart
    | LoadingNoStub String
    | LoadingWithStub String ReferenceSetStub
    | RefSetNotFound String
    | RefSet String ReferenceSet
    | Deleted String


mapPageState :
    ({ uuidString : String, referenceSet : ReferenceSet }
     -> { uuidString : String, referenceSet : ReferenceSet }
    )
    -> PageState
    -> PageState
mapPageState refSetFn focus =
    case focus of
        FailedToStart ->
            focus

        LoadingNoStub _ ->
            focus

        LoadingWithStub _ _ ->
            focus

        RefSetNotFound _ ->
            focus

        RefSet uuidString referenceSet ->
            { uuidString = uuidString, referenceSet = referenceSet }
                |> refSetFn
                |> (\idRefSet -> RefSet idRefSet.uuidString idRefSet.referenceSet)

        Deleted _ ->
            focus


type alias DisplaySettings =
    { pdbCodes : Bool }



-- }}}
-- {{{ INIT


type alias Params =
    { uuid : String }


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared { params } =
    case Shared.getRunState shared of
        Just runState ->
            ( case
                Dict.get params.uuid runState.referenceSets
                    |> Maybe.map ReferenceSet.storedReferenceSetToStub
              of
                Just stub ->
                    { key = shared.key
                    , pageState = LoadingWithStub params.uuid stub
                    , displaySettings = { pdbCodes = False }
                    , device = classifyDevice shared
                    }

                Nothing ->
                    { key = shared.key
                    , pageState = LoadingNoStub params.uuid
                    , displaySettings = { pdbCodes = False }
                    , device = classifyDevice shared
                    }
            , ReferenceSet.getReferenceSetForRefSetDetails { uuidString = params.uuid }
            )

        Nothing ->
            ( { key = shared.key
              , pageState = FailedToStart
              , displaySettings = { pdbCodes = False }
              , device = classifyDevice shared
              }
            , Cmd.none
            )



-- }}}
-- {{{ UPDATE


type Msg
    = SetFocus { uuidString : String, refSetValue : Value }
    | DeleteFocussedReferenceSet String Buttons.DangerStatus
    | TogglePdbCodesFold


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetFocus { uuidString, refSetValue } ->
            case Codec.decodeValue ReferenceSet.codec refSetValue of
                Ok refSet ->
                    ( { model | pageState = RefSet uuidString refSet }
                    , plotCommands model.device refSet
                    )

                Err _ ->
                    ( { model | pageState = RefSetNotFound uuidString }, Cmd.none )

        DeleteFocussedReferenceSet uuid dangerStatus ->
            if Buttons.isConfirmed dangerStatus then
                ( { model | pageState = Deleted uuid }
                , Cmd.batch
                    [ ReferenceSet.deleteReferenceSet { uuidString = uuid }
                    , navigate model.key Route.DeStress__ReferenceSets
                    ]
                )

            else
                ( { model
                    | pageState =
                        mapPageState
                            (\{ uuidString, referenceSet } ->
                                { uuidString = uuidString
                                , referenceSet =
                                    { referenceSet | deleteStatus = dangerStatus }
                                }
                            )
                            model.pageState
                  }
                , Cmd.none
                )

        TogglePdbCodesFold ->
            ( { model
                | displaySettings =
                    { pdbCodes = not model.displaySettings.pdbCodes
                    }
              }
            , Cmd.none
            )


plotCommands : Device -> ReferenceSet -> Cmd msg
plotCommands device referenceSet =
    Cmd.batch
        [ Plots.vegaPlot <|
            { plotId = "composition"
            , spec =
                Metrics.createCompositionSpec
                    device
                    referenceSet.aggregateData
                    Nothing
            }
        , Plots.vegaPlot <|
            { plotId = "torsionAngles"
            , spec =
                Metrics.createTorsionAngleSpec
                    device
                    Nothing
                    referenceSet.metrics
            }
        , Plots.vegaPlot <|
            { plotId = "metricsHistograms"
            , spec =
                Metrics.createAllHistogramsSpec
                    device
                    []
                    (referenceSet.metrics
                        |> List.map Metrics.makeHistPlotData
                    )
            }
        ]


save : Model -> Shared.Model -> Shared.Model
save model shared =
    case model.pageState of
        Deleted uuidString ->
            Shared.mapRunState
                (\runState ->
                    { runState
                        | referenceSets =
                            Dict.remove uuidString runState.referenceSets
                        , saveStateRequested = True
                    }
                )
                shared

        _ ->
            shared


load : Shared.Model -> Model -> ( Model, Cmd Msg )
load shared model =
    ( { model
        | device = classifyDevice shared
      }
    , case model.pageState of
        RefSet _ refSet ->
            plotCommands model.device refSet

        _ ->
            Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    setFocussedReferenceSet SetFocus



-- }}}
-- {{{ VIEW


view : Model -> Document Msg
view model =
    { title = "Reference Set Details"
    , body = [ bodyView model ]
    }


bodyView : Model -> Element Msg
bodyView model =
    el [ centerX, width (fill |> maximum 900) ] <|
        case model.pageState of
            FailedToStart ->
                paragraph []
                    [ text
                        """Page failed to launch, you should never be able to see this
                        screen, but here you are! Please try refreshing your browser and
                        if the problem persists, please report this bug. You can find
                        details of how to report a bug on the home page.
                        """
                    ]

            LoadingNoStub uuidString ->
                el [] ("Loading reference set (id: " ++ uuidString ++ ")..." |> text)

            LoadingWithStub uuidString stub ->
                sectionColumn
                    [ simpleDetails uuidString stub
                    , paragraph [ Font.bold ] [ text "Loading overview..." ]
                    ]

            RefSetNotFound referenceSetUuid ->
                el [] ("A reference set with ID \"" ++ referenceSetUuid ++ "\" was not found." |> text)

            RefSet uuidString referenceSet ->
                sectionColumn
                    [ fullDetails
                        uuidString
                        model.displaySettings
                        referenceSet
                    ]

            Deleted uuidString ->
                el [] ("This reference set (" ++ uuidString ++ ") has been deleted." |> text)


sectionColumn : List (Element msg) -> Element msg
sectionColumn =
    column [ spacing 12, width fill ]


simpleDetails :
    String
    ->
        { a
            | name : String
            , description : String
            , aggregateData : Metrics.AggregateData
            , deleteStatus : Buttons.DangerStatus
        }
    -> Element Msg
simpleDetails uuidString refSetOrStub =
    sectionColumn
        [ wrappedRow [ spacing 10 ]
            [ paragraph [] [ Style.h1 <| text "Reference Set Details" ]
            , row [ spacing 10 ]
                [ Buttons.linkButton
                    { label = text "Back"
                    , route = Route.DeStress__ReferenceSets
                    }
                , Buttons.dangerousButton
                    { label = text "Delete"
                    , confirmText = "Are you sure you want to delete this reference set?"
                    , status = refSetOrStub.deleteStatus
                    , dangerousMsg = DeleteFocussedReferenceSet uuidString
                    }
                ]
            ]
        , sectionColumn
            [ Style.h2 <| text "Name"
            , text refSetOrStub.name
            , Style.h2 <|
                text "Description"
            , paragraph
                []
                [ text refSetOrStub.description ]
            , Style.h2 <|
                text "Aggregate Data"
            , aggregateDataTable refSetOrStub.aggregateData
            ]
        ]


fullDetails :
    String
    -> DisplaySettings
    ->
        { a
            | name : String
            , description : String
            , aggregateData : Metrics.AggregateData
            , deleteStatus : Buttons.DangerStatus
            , metrics : List RefSetMetrics
        }
    -> Element Msg
fullDetails uuidString displaySettings referenceSet =
    sectionColumn
        [ simpleDetails uuidString referenceSet
        , Style.h2 <|
            text "Overview"
        , paragraph
            []
            [ text "This reference set contains "
            , referenceSet.metrics
                |> List.length
                |> String.fromInt
                |> text
            , text " structures."
            ]
        , Folds.sectionFoldView
            { foldVisible = displaySettings.pdbCodes
            , title = "Structure Files"
            , toggleMsg = TogglePdbCodesFold
            , contentView =
                paragraph []
                    [ List.map .pdbCode referenceSet.metrics
                        |> String.join " "
                        |> text
                    ]
            }
        , referenceOverview
        ]


roundFloatText : Float -> Int -> Element msg
roundFloatText value digits =
    text (Round.round digits value)


aggregateDataTable : Metrics.AggregateData -> Element msg
aggregateDataTable aggregateData =
    let
        collatedData =
            [ aggregateData.hydrophobicFitness
            , aggregateData.isoelectricPoint
            , aggregateData.numberOfResidues
            , aggregateData.packingDensity
            , aggregateData.budeFFTotalEnergy
            , aggregateData.evoEFTotalEnergy
            , aggregateData.dfireTotalEnergy
            , aggregateData.rosettaTotalEnergy
            , aggregateData.aggrescan3dTotalValue
            ]
                |> List.filterMap identity
                |> List.map2 Tuple.pair
                    [ "Hydrophobic Fitness"
                    , "Isoelectric Point"
                    , "Number Of Residues"
                    , "Packing Density"
                    , "BUDE FF Total Energy"
                    , "EvoEF2 Total Energy"
                    , "DFIRE2 Total Energy"
                    , "Rosetta Total Energy"
                    , "Aggrescan3d Total Value"
                    ]
    in
    column
        [ width fill ]
        (row [ padding 5, width fill, Border.widthXY 0 2, Font.bold ]
            [ el [ width <| fillPortion 1 ] <| text "Metric"
            , el [ width <| fillPortion 1, Font.alignRight ] <| text "Mean"
            , el [ width <| fillPortion 1, Font.alignRight ] <| text "Median"
            , el [ width <| fillPortion 1, Font.alignRight ] <| text "Std Dev"
            ]
            :: List.map rowView collatedData
        )


rowView : ( String, Metrics.MeanMedAndStdDev ) -> Element msg
rowView ( name, { mean, median, stdDev } ) =
    row [ padding 5, spacing 5, width fill, Border.widthEach { top = 0, bottom = 1, left = 0, right = 0 } ]
        [ paragraph [ width <| fillPortion 1 ] [ text name ]
        , el [ width <| fillPortion 1, Font.alignRight ] <| roundFloatText mean 2
        , el [ width <| fillPortion 1, Font.alignRight ] <| roundFloatText median 2
        , el [ width <| fillPortion 1, Font.alignRight ] <| roundFloatText stdDev 2
        ]



-- {{{ Overview Plots


referenceOverview : Element msg
referenceOverview =
    sectionColumn
        [ compositionView
        , torsionAnglesView
        , metricsHistogramsView
        ]


compositionView : Element msg
compositionView =
    column
        [ width fill ]
        [ Style.h3 <| text "Composition"
        , Keyed.el [ centerX ]
            ( "composition"
            , Html.div
                [ HAtt.id "composition"
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


torsionAnglesView : Element msg
torsionAnglesView =
    column
        [ width fill ]
        [ Style.h3 <| text "Backbone Torsion Angles"
        , Keyed.el [ centerX ]
            ( "torsionAngles"
            , Html.div
                [ HAtt.id "torsionAngles"
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


metricsHistogramsView : Element msg
metricsHistogramsView =
    column [ width fill ]
        [ Style.h3 <| text "Metrics Histograms"
        , Keyed.el [ centerX ]
            ( "metricsHistograms"
            , Html.div
                [ HAtt.id "metricsHistograms"
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



-- }}}
-- }}}
