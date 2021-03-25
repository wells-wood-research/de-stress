module Pages.ReferenceSets.New exposing (Model, Msg, Params, page)

import BigStructure.Object.Aggrescan3DResults as Aggrescan3DResults
import BigStructure.Object.BiolUnit as BiolUnit
import BigStructure.Object.BudeFFResults as BudeFFResults
import BigStructure.Object.DFIRE2Results as DFIRE2Results
import BigStructure.Object.EvoEF2Results as EvoEF2Results
import BigStructure.Object.Pdb as Pdb
import BigStructure.Object.RosettaResults as RosettaResults
import BigStructure.Object.State as State
import BigStructure.Query as Query
import Browser.Navigation as Nav
import Codec
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import RemoteData exposing (RemoteData)
import Set exposing (Set)
import Shared
import Shared.Buttons as Buttons
import Shared.Error as Error
import Shared.Metrics as Metrics exposing (RefSetMetrics)
import Shared.ReferenceSet as ReferenceSet exposing (ReferenceSet)
import Shared.ResourceUuid as ResourceUuid exposing (ResourceUuid)
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



-- {{{ MODEL


type alias Model =
    { mResourceUuid : Maybe ResourceUuid
    , pageState : PageState
    , referenceSets : Dict String ReferenceSet.StoredReferenceSet
    , navKey : Nav.Key
    , pageErrors : List Error.Error
    }


type PageState
    = ChoosingRefSetType ConstructionMethod
    | BuildingReferenceSet NewReferenceSet
    | CompletedBuilding NewReferenceSet


type ConstructionMethod
    = Default DefaultReferenceSet
    | PdbCodeList PdbCodeListModel


type DefaultReferenceSet
    = Top500


type alias PdbCodeListModel =
    { mName : Maybe String
    , mDescription : Maybe String
    , rawPdbCodeListInput : String
    , pdbCodeList : Set String
    , malformedPdbCodes : Set String
    , remoteData : ReferenceSetRemoteData
    }


defaultCodeListModel : PdbCodeListModel
defaultCodeListModel =
    { mName = Nothing
    , mDescription = Nothing
    , rawPdbCodeListInput = ""
    , pdbCodeList = Set.empty
    , malformedPdbCodes = Set.empty
    , remoteData = RemoteData.NotAsked
    }


type alias NewReferenceSet =
    { id : String
    , name : String
    , description : String
    , pdbCodes : Set String
    , failedCodes : Set String
    , metricsRemoteData : MetricsRemoteData
    }


type alias MetricsRemoteData =
    { basicMetrics : Maybe BasicMetricsRemoteData
    , budeFFTotal : Maybe BudeFFTotalRemoteData
    , evoEF2Total : Maybe EvoEF2TotalRemoteData
    , dfire2Total : Maybe Dfire2TotalRemoteData
    , rosettaTotal : Maybe RosettaTotalRemoteData
    , aggrescan3DTotal : Maybe Aggrescan3DTotalRemoteData
    }


initialMetricsRemoteData : MetricsRemoteData
initialMetricsRemoteData =
    { basicMetrics = Nothing
    , budeFFTotal = Nothing
    , evoEF2Total = Nothing
    , rosettaTotal = Nothing
    , dfire2Total = Nothing
    , aggrescan3DTotal = Nothing
    }


getBuildProgress : NewReferenceSet -> { max : Int, current : Int }
getBuildProgress { metricsRemoteData } =
    let
        maybeToInt : Maybe a -> Int
        maybeToInt mA =
            case mA of
                Just _ ->
                    1

                Nothing ->
                    0

        progressValues =
            [ maybeToInt metricsRemoteData.basicMetrics
            , maybeToInt metricsRemoteData.budeFFTotal
            , maybeToInt metricsRemoteData.dfire2Total
            , maybeToInt metricsRemoteData.evoEF2Total
            , maybeToInt metricsRemoteData.rosettaTotal
            , maybeToInt metricsRemoteData.aggrescan3DTotal
            ]
    in
    { max = List.length progressValues
    , current = List.sum progressValues
    }


constructionMethodToString : ConstructionMethod -> String
constructionMethodToString constructionMethod =
    case constructionMethod of
        Default refSet ->
            case refSet of
                Top500 ->
                    top500.name

        PdbCodeList _ ->
            "PDB Subset"



-- }}}
-- {{{ INIT


type alias Params =
    ()


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared _ =
    case Shared.getRunState shared of
        Just runState ->
            ( { mResourceUuid = Just runState.resourceUuid
              , pageState = ChoosingRefSetType <| Default Top500
              , referenceSets = runState.referenceSets
              , navKey = shared.key
              , pageErrors = []
              }
            , Cmd.none
            )

        Nothing ->
            ( { mResourceUuid = Nothing
              , pageState = ChoosingRefSetType <| Default Top500
              , referenceSets = Dict.empty
              , navKey = shared.key
              , pageErrors = []
              }
            , Cmd.none
            )



-- }}}
-- {{{ UPDATE


type Msg
    = UpdatedConstructionMethod ConstructionMethod
    | ClickedDownloadRefSet
    | GotBasicMetrics BasicMetricsRemoteData
    | GotBudeFFMetrics BudeFFTotalRemoteData
    | GotEvoEF2Metrics EvoEF2TotalRemoteData
    | GotDfire2Metrics Dfire2TotalRemoteData
    | GotRosettaMetrics RosettaTotalRemoteData
    | GotAggrescan3DMetrics Aggrescan3DTotalRemoteData
      -- | UpdatedName NewPdbCodeListParams String
      -- | UpdatedDescription NewPdbCodeListParams String
      -- | UpdatedPdbCodes NewPdbCodeListParams String
      -- | ClickedDownloadPreferredSubset
      -- | GotPreferredSubsetData ReferenceSetRemoteData
    | ClickedCreateReferenceSet
    | ClickedCancelCreate
    | ClearPageErrors


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdatedConstructionMethod newConstructionMethod ->
            ( { model | pageState = ChoosingRefSetType newConstructionMethod }
            , Cmd.none
            )

        ClickedDownloadRefSet ->
            case model.pageState of
                ChoosingRefSetType (Default refSet) ->
                    let
                        newRefSet =
                            case refSet of
                                Top500 ->
                                    top500
                    in
                    ( { model
                        | pageState =
                            BuildingReferenceSet
                                newRefSet
                      }
                    , Cmd.batch
                        [ basicMetricsQuery newRefSet.pdbCodes
                            |> Graphql.Http.queryRequest
                                "http://127.0.0.1:8181/graphql"
                            |> Graphql.Http.send
                                (RemoteData.fromResult >> GotBasicMetrics)
                        , budeFFTotalQuery newRefSet.pdbCodes
                            |> Graphql.Http.queryRequest
                                "http://127.0.0.1:8181/graphql"
                            |> Graphql.Http.send
                                (RemoteData.fromResult >> GotBudeFFMetrics)
                        , evoEF2TotalQuery newRefSet.pdbCodes
                            |> Graphql.Http.queryRequest
                                "http://127.0.0.1:8181/graphql"
                            |> Graphql.Http.send
                                (RemoteData.fromResult >> GotEvoEF2Metrics)
                        , dfire2TotalQuery newRefSet.pdbCodes
                            |> Graphql.Http.queryRequest
                                "http://127.0.0.1:8181/graphql"
                            |> Graphql.Http.send
                                (RemoteData.fromResult >> GotDfire2Metrics)
                        , rosettaTotalQuery newRefSet.pdbCodes
                            |> Graphql.Http.queryRequest
                                "http://127.0.0.1:8181/graphql"
                            |> Graphql.Http.send
                                (RemoteData.fromResult >> GotRosettaMetrics)
                        , aggrescan3DTotalQuery newRefSet.pdbCodes
                            |> Graphql.Http.queryRequest
                                "http://127.0.0.1:8181/graphql"
                            |> Graphql.Http.send
                                (RemoteData.fromResult >> GotAggrescan3DMetrics)
                        ]
                    )

                _ ->
                    Debug.todo "Add error catching."

        GotBasicMetrics remoteData ->
            case model.pageState of
                BuildingReferenceSet ({ metricsRemoteData } as newRefSet) ->
                    ( { model
                        | pageState =
                            BuildingReferenceSet
                                { newRefSet
                                    | metricsRemoteData =
                                        { metricsRemoteData
                                            | basicMetrics = Just remoteData
                                        }
                                }
                      }
                    , Cmd.none
                    )

                _ ->
                    Debug.todo "Add error catching"

        GotBudeFFMetrics remoteData ->
            case model.pageState of
                BuildingReferenceSet ({ metricsRemoteData } as newRefSet) ->
                    ( { model
                        | pageState =
                            BuildingReferenceSet
                                { newRefSet
                                    | metricsRemoteData =
                                        { metricsRemoteData
                                            | budeFFTotal = Just remoteData
                                        }
                                }
                      }
                    , Cmd.none
                    )

                _ ->
                    Debug.todo "Add error catching"

        GotEvoEF2Metrics remoteData ->
            case model.pageState of
                BuildingReferenceSet ({ metricsRemoteData } as newRefSet) ->
                    ( { model
                        | pageState =
                            BuildingReferenceSet
                                { newRefSet
                                    | metricsRemoteData =
                                        { metricsRemoteData
                                            | evoEF2Total = Just remoteData
                                        }
                                }
                      }
                    , Cmd.none
                    )

                _ ->
                    Debug.todo "Add error catching"

        GotDfire2Metrics remoteData ->
            case model.pageState of
                BuildingReferenceSet ({ metricsRemoteData } as newRefSet) ->
                    ( { model
                        | pageState =
                            BuildingReferenceSet
                                { newRefSet
                                    | metricsRemoteData =
                                        { metricsRemoteData
                                            | dfire2Total = Just remoteData
                                        }
                                }
                      }
                    , Cmd.none
                    )

                _ ->
                    Debug.todo "Add error catching"

        GotRosettaMetrics remoteData ->
            case model.pageState of
                BuildingReferenceSet ({ metricsRemoteData } as newRefSet) ->
                    ( { model
                        | pageState =
                            BuildingReferenceSet
                                { newRefSet
                                    | metricsRemoteData =
                                        { metricsRemoteData
                                            | rosettaTotal = Just remoteData
                                        }
                                }
                      }
                    , Cmd.none
                    )

                _ ->
                    Debug.todo "Add error catching"

        GotAggrescan3DMetrics remoteData ->
            case model.pageState of
                BuildingReferenceSet ({ metricsRemoteData } as newRefSet) ->
                    ( { model
                        | pageState =
                            BuildingReferenceSet
                                { newRefSet
                                    | metricsRemoteData =
                                        { metricsRemoteData
                                            | aggrescan3DTotal = Just remoteData
                                        }
                                }
                      }
                    , Cmd.none
                    )

                _ ->
                    Debug.todo "Add error catching"

        -- GotMetricsBatch _ ->
        --     Error.updateWithError
        --         ClearPageErrors
        --         { model | pageState = ChoosingRefSetType (Default Top500) }
        --         { title = "Failed to create reference set"
        --         , details =
        --             """Failed to create a new reference set. It looks like we
        --             had trouble downloading the data from the server. Check your
        --             internet connection and refresh your browser. If this
        --             problem persists, report it as a bug. See the home page for
        --             details on how to do this.
        --             """
        --         , severity = Error.Medium
        --         }
        ClickedCreateReferenceSet ->
            -- case model.pageState of
            --     CompletedBuilding newRefSet ->
            ( model, Cmd.none )

        ClickedCancelCreate ->
            Debug.todo "Add cancel option"

        -- -- Preferred State Subset
        -- UpdatedName params name ->
        --     if String.isEmpty name then
        --         ( { model
        --             | newReferenceSet =
        --                 { params | mName = Nothing }
        --                     |> NewPdbCodeList
        --           }
        --         , Cmd.none
        --         )
        --     else
        --         ( { model
        --             | newReferenceSet =
        --                 { params | mName = Just name }
        --                     |> NewPdbCodeList
        --           }
        --         , Cmd.none
        --         )
        -- UpdatedDescription params description ->
        --     if String.isEmpty description then
        --         ( { model
        --             | newReferenceSet =
        --                 { params | mDescription = Nothing }
        --                     |> NewPdbCodeList
        --           }
        --         , Cmd.none
        --         )
        --     else
        --         ( { model
        --             | newReferenceSet =
        --                 { params | mDescription = Just description }
        --                     |> NewPdbCodeList
        --           }
        --         , Cmd.none
        --         )
        -- UpdatedPdbCodes params rawCodes ->
        --     let
        --         isPdbCode str =
        --             (String.length str == 4)
        --                 && (String.toList str
        --                         |> List.all Char.isAlphaNum
        --                    )
        --         ( good, malformed ) =
        --             String.words rawCodes
        --                 |> Set.fromList
        --                 |> Set.partition isPdbCode
        --     in
        --     ( { model
        --         | newReferenceSet =
        --             { params
        --                 | rawPdbCodeListInput = rawCodes
        --                 , pdbCodeList = good
        --                 , malformedPdbCodes = malformed
        --             }
        --                 |> NewPdbCodeList
        --       }
        --     , Cmd.none
        --     )
        -- ClickedDownloadPreferredSubset ->
        --     case model.newReferenceSet of
        --         NewPdbCodeList params ->
        --             ( { model
        --                 | newReferenceSet =
        --                     NewPdbCodeList
        --                         { params
        --                             | remoteData = RemoteData.Loading
        --                         }
        --               }
        --             , ReferenceSet.queryToCmd
        --                 (ReferenceSet.preferredStatesSubsetQuery params.pdbCodeList)
        --                 GotPreferredSubsetData
        --             )
        --         NewHighResBiolUnit _ ->
        --             Error.updateWithError
        --                 ClearPageErrors
        --                 model
        --                 { title = "Unexpected reference set type"
        --                 , details =
        --                     """Failed to create a new reference set. The type of
        --                         reference set you're creating shouldn't have allowed you
        --                         to click this button!  Try refreshing your browser to
        --                         fix this, or if it persists, report it as a bug.  See
        --                         the home page for details on how to do this.
        --                     """
        --                 , severity = Error.Medium
        --                 }
        -- GotPreferredSubsetData remoteData ->
        --     case ( model.newReferenceSet, model.mResourceUuid, remoteData ) of
        --         ( NewPdbCodeList params, Just resourceUuid, RemoteData.Success metrics ) ->
        --             let
        --                 { uuidString, nextResourceUuid } =
        --                     ResourceUuid.toString
        --                         resourceUuid
        --                 referenceSet =
        --                     ReferenceSet.PdbCodeList
        --                         { metrics = metrics
        --                         , aggregateData = Metrics.createAggregateData metrics
        --                         , name =
        --                             Maybe.withDefault "NAME"
        --                                 params.mName
        --                         , description =
        --                             Maybe.withDefault "DESCRIPTION"
        --                                 params.mDescription
        --                         , pdbCodeList = params.pdbCodeList
        --                         , deleteStatus = Buttons.initDangerStatus
        --                         }
        --             in
        --             ( { model
        --                 | newReferenceSet = NewHighResBiolUnit RemoteData.NotAsked
        --                 , mResourceUuid = Just nextResourceUuid
        --                 , referenceSets =
        --                     Dict.insert
        --                         uuidString
        --                         (referenceSet
        --                             |> ReferenceSet.createReferenceSetStub
        --                             |> ReferenceSet.storeReferenceSetStubLocally
        --                         )
        --                         model.referenceSets
        --               }
        --             , Cmd.batch
        --                 [ ReferenceSet.storeReferenceSet
        --                     { uuidString = uuidString
        --                     , referenceSet =
        --                         Codec.encoder
        --                             ReferenceSet.codec
        --                             referenceSet
        --                     }
        --                 , navigate model.navKey Route.ReferenceSets
        --                 ]
        --             )
        --         _ ->
        --             Error.updateWithError
        --                 ClearPageErrors
        --                 model
        --                 { title = "Failed to create reference set"
        --                 , details =
        --                     """Failed to create a new reference set. Try refreshing your
        --                     browser to fix this, or if it persists, report it as a bug.
        --                     See the home page for details on how to do this.
        --                     """
        --                 , severity = Error.Medium
        --                 }
        ClearPageErrors ->
            ( { model | pageErrors = [] }
            , Cmd.none
            )


save : Model -> Shared.Model -> Shared.Model
save model shared =
    let
        updatedShared =
            Error.updateSharedModelErrors model shared
    in
    case model.mResourceUuid of
        Just resourceUuid ->
            Shared.mapRunState
                (\runState ->
                    { runState
                        | resourceUuid = resourceUuid
                        , referenceSets = model.referenceSets
                        , saveStateRequested = True
                    }
                )
                updatedShared

        Nothing ->
            updatedShared


load : Shared.Model -> Model -> ( Model, Cmd Msg )
load shared model =
    case Shared.getRunState shared of
        Just runState ->
            ( { model
                | mResourceUuid = Just runState.resourceUuid
                , referenceSets = runState.referenceSets
              }
            , Cmd.none
            )

        Nothing ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- }}}
-- {{{ VIEW


view : Model -> Document Msg
view model =
    { title = "Create New Reference Set"
    , body = [ bodyView model ]
    }


bodyView : Model -> Element Msg
bodyView model =
    column
        [ Style.pageWidths.singleColumn
        , centerX
        , spacing 30
        ]
        [ text "Create New Reference Set"
            |> Style.h1
            |> el [ centerX ]
        , case model.pageState of
            ChoosingRefSetType currentChoice ->
                column [ spacing 15 ]
                    [ row [ centerX, spacing 15 ] <|
                        (List.map
                            (refSetTypeSelector currentChoice)
                            [ Default Top500
                            ]
                            |> List.intersperse (text "|")
                        )
                    , case currentChoice of
                        Default refSet ->
                            defaultRefSetView refSet

                        _ ->
                            Debug.todo "Add option"
                    ]

            BuildingReferenceSet newRefSet ->
                buildingProgressView newRefSet

            CompletedBuilding newRefSet ->
                completedBuildingView newRefSet
        ]


refSetTypeSelector : ConstructionMethod -> ConstructionMethod -> Element Msg
refSetTypeSelector current option =
    constructionMethodToString option
        |> text
        |> el
            ((if constructionMethodToString current == constructionMethodToString option then
                [ Font.underline ]

              else
                [ Events.onClick <| UpdatedConstructionMethod option ]
             )
                ++ [ pointer ]
            )


defaultRefSetView : DefaultReferenceSet -> Element Msg
defaultRefSetView defaultSet =
    let
        refSet =
            case defaultSet of
                Top500 ->
                    top500
    in
    column [ width fill, spacing 30 ]
        [ paragraph []
            [ text refSet.description
            ]
        , Buttons.alwaysActiveButton
            { clickMsg =
                ClickedDownloadRefSet
            , label = text "Download Reference Set"
            , pressed = False
            }
        ]


buildingProgressView : NewReferenceSet -> Element msg
buildingProgressView newRefSet =
    column
        [ spacing 10, width fill ]
        [ paragraph
            []
            [ text "Downloading reference set..." ]
        , getBuildProgress newRefSet
            |> Style.progressBar
        ]


completedBuildingView : NewReferenceSet -> Element msg
completedBuildingView newRefSet =
    column
        [ spacing 15, width fill ]
        ([ paragraph
            []
            [ text "Finished downloading data." ]
         , getBuildProgress newRefSet
            |> Style.progressBar
         , paragraph
            []
            [ text "Downloaded metrics for "
            , text " structures."
            ]
         ]
            ++ (if not <| Set.isEmpty newRefSet.failedCodes then
                    [ Style.h2 <| text "Warning"
                    , paragraph
                        []
                        [ text
                            """Big Structure does not contain metrics for all
                            structures, as it is either impractical or impossible to run
                            the metrics for certain files. The following files are
                            currently unavailable:
                            """
                        ]
                    , paragraph
                        []
                        [ Set.toList newRefSet.failedCodes
                            |> String.join " "
                            |> text
                        ]
                    ]

                else
                    []
               )
        )



-- newPdbCodeListView : NewPdbCodeListParams -> Element Msg
-- newPdbCodeListView params =
--     column [ width fill, spacing 30 ]
--         [ paragraph []
--             [ text
--                 """Create a reference set from a list of PDB codes. The biological unit
--                 of the structure, as defined on PDBe as assembly 1, will be used to
--                 create the reference set.
--                 """
--             ]
--         , Input.text
--             Style.textInputStyle
--             { onChange = UpdatedName params
--             , text = Maybe.withDefault "" params.mName
--             , placeholder = Nothing
--             , label =
--                 Input.labelAbove []
--                     (Style.h2 <| text "Name")
--             }
--         , Input.multiline
--             Style.textInputStyle
--             { onChange = UpdatedDescription params
--             , text = Maybe.withDefault "" params.mDescription
--             , placeholder = Nothing
--             , label =
--                 Input.labelAbove []
--                     (Style.h2 <| text "Description")
--             , spellcheck = False
--             }
--         , Input.multiline
--             (Style.textInputStyle
--                 ++ [ height <| px 200 ]
--             )
--             { onChange = UpdatedPdbCodes params
--             , text = params.rawPdbCodeListInput
--             , placeholder = Nothing
--             , label =
--                 Input.labelAbove []
--                     (Style.h2 <|
--                         text "PDB Codes (whitespace separated)"
--                     )
--             , spellcheck = True
--             }
--         , paragraph []
--             [ "Found "
--                 ++ (Set.size params.pdbCodeList |> String.fromInt)
--                 ++ " PDB codes. "
--                 ++ (Set.size params.malformedPdbCodes |> String.fromInt)
--                 ++ " "
--                 ++ (if Set.size params.malformedPdbCodes > 1 then
--                         "entries do"
--                     else
--                         "entry does"
--                    )
--                 ++ " not look a like PDB code:\n"
--                 ++ (Set.toList params.malformedPdbCodes |> String.join " ")
--                 |> text
--             ]
--         , let
--             validName =
--                 case params.mName of
--                     Nothing ->
--                         False
--                     Just _ ->
--                         True
--             validDescription =
--                 case params.mDescription of
--                     Nothing ->
--                         False
--                     Just _ ->
--                         True
--             complete =
--                 validName && validDescription && (Set.isEmpty >> not) params.pdbCodeList
--           in
--           Buttons.conditionalButton
--             { clickMsg =
--                 Just ClickedDownloadPreferredSubset
--             , label = text "Create Reference Set"
--             , isActive = complete
--             }
--         ]
-- }}}
-- {{{ New Reference Set Queries


type alias ReferenceSetRemoteData =
    RemoteData (Graphql.Http.Error (List RefSetMetrics)) (List RefSetMetrics)


type alias BasicMetrics =
    { pdbCode : String
    , composition : Dict String Float
    , torsionAngles : Dict String ( Float, Float, Float )
    , hydrophobicFitness : Maybe Float
    , isoelectricPoint : Float
    , mass : Float
    , numberOfResidues : Int
    , packingDensity : Float
    }


type alias BasicMetricsRemoteData =
    RemoteData (Graphql.Http.Error (List BasicMetrics)) (List BasicMetrics)


basicMetricsQuery : Set String -> SelectionSet (List BasicMetrics) RootQuery
basicMetricsQuery pdbCodeList =
    Query.preferredStatesSubset
        (\optionals -> { optionals | stateNumber = Absent })
        { codes = Set.toList pdbCodeList }
        (SelectionSet.succeed BasicMetrics
            |> with
                (State.biolUnit (BiolUnit.pdb Pdb.pdbCode)
                    |> SelectionSet.map
                        (\mmPdbCode ->
                            case mmPdbCode of
                                Just (Just pdbCode) ->
                                    pdbCode

                                _ ->
                                    "Unknown PDB"
                        )
                )
            |> with (SelectionSet.map Metrics.compositionStringToDict State.composition)
            |> with (SelectionSet.map Metrics.torsionAngleStringToDict State.torsionAngles)
            |> with State.hydrophobicFitness
            |> with State.isoelectricPoint
            |> with State.mass
            |> with State.numOfResidues
            |> with State.meanPackingDensity
        )


unwrapMMSS : SelectionSet (Maybe (Maybe (Maybe b))) scope -> SelectionSet (Maybe b) scope
unwrapMMSS =
    SelectionSet.map
        (Maybe.andThen (Maybe.andThen identity))


type alias BudeFFTotal =
    { pdbCode : String
    , budeFFTotal : Maybe Float
    }


type alias BudeFFTotalRemoteData =
    RemoteData (Graphql.Http.Error (List BudeFFTotal)) (List BudeFFTotal)


budeFFTotalQuery : Set String -> SelectionSet (List BudeFFTotal) RootQuery
budeFFTotalQuery pdbCodeList =
    Query.preferredBudeSubset
        (\optionals -> { optionals | stateNumber = Absent })
        { codes = Set.toList pdbCodeList }
        (SelectionSet.succeed BudeFFTotal
            |> with
                (SelectionSet.map (Maybe.withDefault "Unknown PDB")
                    (unwrapMMSS
                        (BudeFFResults.state
                            (State.biolUnit
                                (BiolUnit.pdb
                                    Pdb.pdbCode
                                )
                            )
                        )
                    )
                )
            |> with BudeFFResults.totalEnergy
        )


type alias EvoEF2Total =
    { pdbCode : String
    , evoEF2Total : Maybe Float
    }


type alias EvoEF2TotalRemoteData =
    RemoteData (Graphql.Http.Error (List EvoEF2Total)) (List EvoEF2Total)


evoEF2TotalQuery : Set String -> SelectionSet (List EvoEF2Total) RootQuery
evoEF2TotalQuery pdbCodeList =
    Query.preferredEvoef2Subset
        (\optionals -> { optionals | stateNumber = Absent })
        { codes = Set.toList pdbCodeList }
        (SelectionSet.succeed EvoEF2Total
            |> with
                (SelectionSet.map (Maybe.withDefault "Unknown PDB")
                    (unwrapMMSS
                        (EvoEF2Results.state
                            (State.biolUnit
                                (BiolUnit.pdb
                                    Pdb.pdbCode
                                )
                            )
                        )
                    )
                )
            |> with EvoEF2Results.total
        )


type alias Dfire2Total =
    { pdbCode : String
    , dfire2Total : Maybe Float
    }


type alias Dfire2TotalRemoteData =
    RemoteData (Graphql.Http.Error (List Dfire2Total)) (List Dfire2Total)


dfire2TotalQuery : Set String -> SelectionSet (List Dfire2Total) RootQuery
dfire2TotalQuery pdbCodeList =
    Query.preferredDfire2Subset
        (\optionals -> { optionals | stateNumber = Absent })
        { codes = Set.toList pdbCodeList }
        (SelectionSet.succeed Dfire2Total
            |> with
                (SelectionSet.map (Maybe.withDefault "Unknown PDB")
                    (unwrapMMSS
                        (DFIRE2Results.state
                            (State.biolUnit
                                (BiolUnit.pdb
                                    Pdb.pdbCode
                                )
                            )
                        )
                    )
                )
            |> with DFIRE2Results.total
        )


type alias RosettaTotal =
    { pdbCode : String
    , rosettaTotal : Maybe Float
    }


type alias RosettaTotalRemoteData =
    RemoteData (Graphql.Http.Error (List RosettaTotal)) (List RosettaTotal)


rosettaTotalQuery : Set String -> SelectionSet (List RosettaTotal) RootQuery
rosettaTotalQuery pdbCodeList =
    Query.preferredRosettaSubset
        (\optionals -> { optionals | stateNumber = Absent })
        { codes = Set.toList pdbCodeList }
        (SelectionSet.succeed RosettaTotal
            |> with
                (SelectionSet.map (Maybe.withDefault "Unknown PDB")
                    (unwrapMMSS
                        (RosettaResults.state
                            (State.biolUnit
                                (BiolUnit.pdb
                                    Pdb.pdbCode
                                )
                            )
                        )
                    )
                )
            |> with RosettaResults.totalScore
        )


type alias Aggrescan3DTotal =
    { pdbCode : String
    , aggrescan3DTotal : Maybe Float
    }


type alias Aggrescan3DTotalRemoteData =
    RemoteData (Graphql.Http.Error (List Aggrescan3DTotal)) (List Aggrescan3DTotal)


aggrescan3DTotalQuery : Set String -> SelectionSet (List Aggrescan3DTotal) RootQuery
aggrescan3DTotalQuery pdbCodeList =
    Query.preferredAggrescan3dSubset
        (\optionals -> { optionals | stateNumber = Absent })
        { codes = Set.toList pdbCodeList }
        (SelectionSet.succeed Aggrescan3DTotal
            |> with
                (SelectionSet.map (Maybe.withDefault "Unknown PDB")
                    (unwrapMMSS
                        (Aggrescan3DResults.state
                            (State.biolUnit
                                (BiolUnit.pdb
                                    Pdb.pdbCode
                                )
                            )
                        )
                    )
                )
            |> with Aggrescan3DResults.totalValue
        )



-- }}}
-- {{{ Default Reference Sets


top500 : NewReferenceSet
top500 =
    { id = "top-500"
    , name = "Top 500"
    , description =
        """A set of high-quality structures defined by the Richardson lab. Uses the
        preferred biological unit as defined by PDBe.
        """
    , pdbCodes =
        """
        1ajj 1npk 1aay 1iib 1ifc 1nkd 3pyp 1g3p 1mla 1cru 1a2p 1yac 1qnf 1vsr 1rhs 1ctj
        1mun 3pvi 1tfe 2tps 1aba 1dlf 5hpg 1d2n 1pmi 1qcz 2cba 256b 1mrj 1yge 1whi 1b6g
        2bbk 1gvp 3eip 1bkr 2end 1smd 8abp 1dgf 1qtw 1b3a 1b16 2qwc 2cua 1qlw 1a28 1bs9
        1kuh 1bk0 1ytb 4lzt 1bx7 1flt 1erv 1wap 1toa 1mpg 1qd1 1dps 1jer 2sn3 2msb 119l
        1xnb 1hcl 1uae 1c3w 1hmt 1ejg 4xis 2trx 1cxc 1b4k 2arc 2spc 1bdo 3pte 3sdh 1c90
        1bab 1vca 1gpe 1ush 1a8i 2lis 1qh4 1ah7 1byi 1lmb 1mct 1dhn 1bgf 1cka 2dpm 2cpp
        1qk5 1uch 1ido 1qft 1ten 1bi5 1c3d 1fus 1bhp 1gof 1luc 2knt 1qe3 1gai 1orc 1aqz
        1di6 2ilk 1bs0 1psr 1ayl 1d3v 1cgo 5p21 1ak0 1bf4 2mhr 1bty 2ahj 1dcs 1ben 1b9w
        1atl 1bbh 1cpq 1din 1bqc 1aqu 2tnf 6cel 1qs1 3grs 1bpi 153l 1nls 1bkb 1rcf 1dqs
        1dnl 1bk7 1qre 1qrr 1tud 1swu 1bg6 2bop 1atz 1bfd 1bxo 1msk 1qdd 1cv8 1chd 1gca
        2hmz 1fxd 2baa 1b0u 1byq 1qh8 1rge 1fdr 1zin 2tgi 1cxy 1cs1 1hpm 1qsa 2cpg 1nox
        1mro 1vie 1vfr 9wga 1cex 1vcc 1cjw 2ahj 1ceq 1ads 1qip 1ra9 2fdn 1eco 1gdo 1qau
        1lst 1vhh 16pk 1dci 7rsa 2act 1ute 1qqq 1qcx 8ruc 1bf6 1uxy 7a3h 1qks 1pef 1qfm
        451c 1sbp 1doz 2myr 2por 1nif 1isu 1nzy 1hcr 1elk 1agj 1kap 1bb1 7fd1 1czf 7odc
        1plc 1b8o 1hka 1df4 1lkk 1ek0 1cvl 2ctc 3nul 1dvj 1pda 1mfi 6gsv 1dbw 1aie 1mof
        1ay7 1c75 1mgt 1yve 1bm8 1uro 7atj 1aru 1fvk 1hxn 1dif 1a92 1wab 1gd1 1dxg 1evh
        2hft 1not 1a2z 1edg 1bue 1aqb 1tif 1qnj 1ptf 2cbp 3seb 1nul 2mcm 1aoh 1c02 2bc2
        1fds 1lam 1osa 19hc 2sak 1mjh 3ezm 1qus 1ubp 1bte 1qq5 1dpt 1qhv 1cc8 1cf9 3cla
        1a62 1gso 1qsg 1btk 1kp6 1qjd 1bg2 1aac 1cmb 1a4i 1mro 1ajs 2a0b 1qhf 1bfg 1gdj
        1cyo 2nac 3ebx 1bsm 1a6m 1tx4 1nkr 1d7p 1c5e 2igd 1ek6 1avw 1flt 1ql0 1rie 1thv
        1edm 1fmb 1nar 1ctf 1oaa 1axn 1cxq 1qgq 1tml 1tyv 1mfm 1pdo 1nfn 2dri 1a8d 4pga
        1b67 1bx4 2pvb 1qup 1dos 1jet 2eng 1czp 1mba 1hfe 1lbu 1pgs 1qj4 1tph 2cyp 1ixh
        1amm 1qf9 1amp 1a7s 4eug 3cyr 1aho 1ttb 3hts 1tgs 1bec 2erl 1vns 1qgi 5cyt 1ndd
        1fas 1flm 1bj7 1jhg 1qts 1guq 1xwl 1h2r 1brt 2cpl 1dp7 1sml 1m6p 1cip 1arb 1bkj
        1aop 2nlr 1qu9 1cke 1ccz 2bbk 1cyd 1koe 1kpf 2acy 1es5 1etn 1auo 1akr 1kve 1bd0
        3vub 1gci 1a1y 1bu7 1poa 3sil 1qh5 1c1k 1flp 1xik 1svf 1lcl 1msi 1qh8 1a12 5icb
        1stn 1mol 1amf 1a73 1slu 3chb 1xjo 1c1l 1h2r 1bbz 1egw 1vjs 1hfc 1phn 1mdc 1fkj
        1cem 1atg 1mug 1erx 1qgw 1bw9 1a3a 1a8e 1b5e 1ftr 3chy 1mml 1b6a 1qq4 1b4v 1opd
        1c3p 1tca 3std 1nwp 1cnz 1b0y 1euw 1tc1 1ezm 1htr 1gce 1pym 1bdm 1pcf 1rb9 3bto
        1onc 1kpt 1fna 1tgx 1nbc 1tax 2gar 1c52 1t1d 1beh 3pro 1c24 1cnv 1dfu 1pen 1bu8
        1vfy 1svy 1bqk 2hbg 2rn2 1qb7 1dbg 1ako 1cb0 1iab 2ayh 1ugi 1rzl 1cy5 2pth 1moq
        1cl8 1fnc 1bgc 5nul
        """
            |> String.words
            |> Set.fromList
    , failedCodes = Set.empty
    , metricsRemoteData = initialMetricsRemoteData
    }



-- }}}
