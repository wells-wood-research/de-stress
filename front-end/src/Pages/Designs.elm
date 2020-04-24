module Pages.Designs exposing (Model, Msg, page)

import Ampal
import Codec exposing (Value)
import Design
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import File exposing (File)
import File.Select as FileSelect
import Generated.Params as Params
import Generated.Routes as Routes
import Global
import Ports
import Spa.Page exposing (send)
import Specification exposing (Specification)
import Style exposing (h1, h2)
import Task
import Utils.Spa exposing (Page)


page : Page Params.Designs Model Msg model msg appMsg
page =
    Spa.Page.component
        { title = always "Designs"
        , init = init
        , update = always update
        , subscriptions = always subscriptions
        , view = view
        }



-- {{{ Init


type alias Model =
    { loadingState : DesignLoadingState
    , loadErrors : List String
    , mSelectedSpecification : Maybe Specification
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
    | DeleteDesign String Style.DangerStatus
    | DeleteAllDesigns Style.DangerStatus


update : Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update msg model =
    case msg of
        StructuresRequested ->
            ( model, structureRequested, Cmd.none )

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
            , Cmd.none
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
                                |> Design.NotEditing
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
            , Cmd.none
            )

        DeleteDesign uuidString dangerStatus ->
            ( model
            , Cmd.none
            , Global.DeleteDesign uuidString dangerStatus
                |> send
            )

        DeleteAllDesigns dangerStatus ->
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


structureRequested : Cmd Msg
structureRequested =
    FileSelect.files [ "*/*" ] StructureFilesSelected



-- }}}
-- {{{ Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.specificationForDesignsPage GotSpecification ]



-- }}}
-- {{{ View


view : Utils.Spa.PageContext -> Model -> Element Msg
view { global } model =
    case global of
        Global.Running { designs } ->
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
                        { labelText = buttonLabel
                        , clickMsg = StructuresRequested
                        , isActive = isActive
                        }
                    , Style.dangerousButton
                        { labelText = "Delete All"
                        , confirmText = "Are you sure you want to delete ALL design?"
                        , status = model.deleteAllStatus
                        , dangerousMsg = DeleteAllDesigns
                        }
                    ]
                , let
                    designCardData =
                        designs
                            |> Dict.toList
                            |> List.map
                                (\( k, v ) ->
                                    ( k, Global.storedDesignToStub v )
                                )
                            |> List.map (createDesignCardData model.mSelectedSpecification)
                  in
                  case model.mSelectedSpecification of
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
                                |> column [ spacing 15, width fill ]
                            , h2 <| text "Failed to Meet Specification"
                            , failedSpecification
                                |> List.map designCard
                                |> column [ spacing 15, width fill ]
                            , h2 <| text "No Metrics Available"
                            , noMetrics
                                |> List.map designCard
                                |> column [ spacing 15, width fill ]
                            ]

                    Nothing ->
                        designCardData
                            |> List.map designCard
                            |> column [ spacing 15, width fill ]
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
    Maybe Specification
    -> ( String, Design.DesignStub )
    -> DesignCardData
createDesignCardData mSpecification ( uuidString, designStub ) =
    { uuidString = uuidString
    , designStub = designStub
    , mMeetsSpecification =
        case ( mSpecification, designStub.metricsJobStatus ) of
            ( Just specification, Ports.Complete designMetrics ) ->
                Specification.applySpecification designMetrics specification |> Just

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
    column
        [ padding 15
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
        [ Style.h2 <| text designStub.name
        , text ("Structure file: " ++ designStub.fileName)
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
        , row [ spacing 10, width fill ]
            [ Style.linkButton
                { labelText = "Details"
                , url = Routes.toPath <| Routes.routes.designs_dynamic uuidString
                }
            , Style.dangerousButton
                { labelText = "Delete"
                , confirmText = "Are you sure you want to delete this design?"
                , status = designStub.deleteStatus
                , dangerousMsg = DeleteDesign uuidString
                }
            ]
        ]



-- }}}
