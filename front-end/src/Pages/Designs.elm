module Pages.Designs exposing (Model, Msg, Params, page)

import Biomolecules
import Codec exposing (Value)
import Design
import Dict exposing (Dict)
import File exposing (File)
import File.Select as FileSelect
import Shared
import Shared.Buttons as Buttons
import Shared.Editable as Editable
import Spa.Document exposing (Document)
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
    { loadingState : DesignLoadingState
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
    ( { loadingState = Free
      , loadErrors = []
      , designs =
            case shared.appState of
                Shared.Running { designs } ->
                    designs

                _ ->
                    Dict.empty

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
      --| GotSpecification Value
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
            case rStructuralData of
                Ok _ ->
                    -- Currently this is only checking to see if the file is valid PDB
                    ( { model | loadingState = loadingState }
                    , Cmd.none
                      --, { name =
                      --        String.split "." name
                      --            |> List.head
                      --            |> Maybe.withDefault name
                      --            |> Editable.NotEditing
                      --  , fileName = name
                      --  , pdbString =
                      --        contents
                      --            |> String.lines
                      --            |> List.filter (String.startsWith "ATOM")
                      --            |> String.join "\n"
                      --  , deleteStatus = Buttons.initDangerStatus
                      --  --, metricsJobStatus = Ports.Ready
                      --  --, mMeetsActiveSpecification = Nothing
                      --  }
                    )

                Err (Biomolecules.PdbParseError errorString) ->
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

                Err (Biomolecules.HttpError _) ->
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

        -- GotSpecification specificationValue ->
        --     let
        --         specWithUuidCodec =
        --             Codec.object
        --                 (\uuidString specification ->
        --                     { uuidString = uuidString
        --                     , specification = specification
        --                     }
        --                 )
        --                 |> Codec.field "uuidString" .uuidString Codec.string
        --                 |> Codec.field "specification"
        --                     .specification
        --                     Specification.codec
        --                 |> Codec.buildObject
        --     in
        --     ( { model
        --         | mSelectedSpecification =
        --             Codec.decodeValue specWithUuidCodec specificationValue
        --                 |> Result.toMaybe
        --                 |> Maybe.map .specification
        --       }
        --     , Cmd.none
        --     , Cmd.none
        --     )
        DeleteDesign uuidString dangerStatus ->
            ( model
            , Cmd.none
            )

        DeleteAllDesigns dangerStatus ->
            if Buttons.isConfirmed dangerStatus then
                ( { model | deleteAllStatus = Buttons.initDangerStatus }
                , Cmd.none
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
    shared


load : Shared.Model -> Model -> ( Model, Cmd Msg )
load shared model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- }}}
-- {{{ VIEW


view : Model -> Document Msg
view model =
    { title = "Designs"
    , body = []
    }



-- }}}
