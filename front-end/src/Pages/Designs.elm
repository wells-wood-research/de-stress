module Pages.Designs exposing (Model, Msg, page)

import Ampal
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
import RemoteData as RD
import Spa.Page exposing (send)
import Style exposing (h1, h2)
import Task
import Utils.Spa exposing (Page)


page : Page Params.Designs Model Msg model msg appMsg
page =
    Spa.Page.component
        { title = always "Designs"
        , init = always init
        , update = always update
        , subscriptions = always subscriptions
        , view = view
        }



-- {{{ Init


type alias Model =
    { loadingState : DesignLoadingState
    , loadErrors : List String
    }


type DesignLoadingState
    = LoadingFiles Int Int
    | Free


init : Params.Designs -> ( Model, Cmd Msg, Cmd Global.Msg )
init _ =
    ( { loadingState = Free, loadErrors = [] }
    , Cmd.none
    , Cmd.none
    )



-- }}}
-- {{{ Update


type Msg
    = StructuresRequested
    | StructureFilesSelected File (List File)
    | StructureLoaded String String
    | DeleteDesign String Style.DangerStatus


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
                      , metricsRemoteData = RD.Loading
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

        DeleteDesign uuidString dangerStatus ->
            ( model
            , Cmd.none
            , Global.DeleteDesign uuidString dangerStatus
                |> send
            )


structureRequested : Cmd Msg
structureRequested =
    FileSelect.files [ "*/*" ] StructureFilesSelected



-- }}}
-- {{{ Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- }}}
-- {{{ View


view : Utils.Spa.PageContext -> Model -> Element Msg
view { global } model =
    case global of
        Global.Running { designs } ->
            column [ spacing 15, width fill ]
                [ row [ centerX, spacing 10 ]
                    [ h1 <| text "Designs"
                    , let
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
                      Style.conditionalButton
                        { labelText = buttonLabel
                        , clickMsg = StructuresRequested
                        , isActive = isActive
                        }
                    ]
                , designs
                    |> Dict.toList
                    |> List.map
                        (\( k, v ) ->
                            ( k, Global.storedDesignToStub v )
                        )
                    |> List.map designCard
                    |> column [ spacing 15, width fill ]
                ]

        Global.FailedToLaunch _ ->
            Debug.todo "Add common state page"


designCard : ( String, Design.DesignStub ) -> Element Msg
designCard ( uuidString, designStub ) =
    let
        mMeetsSpecification =
            Nothing

        --     case ( mSpecification, design.metricsRemoteData ) of
        --         ( Just specification, RD.Success designMetrics ) ->
        --             applySpecification sequenceStrings designMetrics specification |> Just
        --         _ ->
        --             Nothing
    in
    column
        [ padding 15
        , spacing 10
        , width fill
        , Background.color Style.colorPalette.c5
        , case mMeetsSpecification of
            Nothing ->
                Border.color Style.colorPalette.black

            Just False ->
                Border.color Style.colorPalette.red

            Just True ->
                Border.color Style.colorPalette.c3
        , Border.width 4
        , Border.rounded 10
        ]
        [ Style.h2 <| text designStub.name
        , text ("Structure file: " ++ designStub.fileName)
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
