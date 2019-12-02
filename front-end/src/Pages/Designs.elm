module Pages.Designs exposing
    ( Model
    , Msg
    , page
    )

import Ampal
import Application.Page as Page
import Design
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import File exposing (File)
import File.Select as FileSelect
import Global
import RemoteData as RD exposing (RemoteData)
import Style exposing (h1)
import Task


type alias Model =
    { loadingState : DesignLoadingState
    , loadErrors : List String
    }


type DesignLoadingState
    = Loading Int Int
    | Free


type Msg
    = StructuresRequested
    | StructureFilesSelected File (List File)
    | StructureLoaded String String
    | DeleteDesign String Style.DangerStatus
    | NoOp


page =
    Page.component
        { title = title
        , init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


title : Global.Model -> Model -> String
title _ _ =
    "Designs"



-- {{{ Init


init : Global.Model -> () -> ( Model, Cmd Msg, Cmd Global.Msg )
init _ _ =
    ( { loadingState = Free, loadErrors = [] }
    , Cmd.none
    , Cmd.none
    )



-- }}}
-- {{{ Update


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update _ msg model =
    case msg of
        StructuresRequested ->
            ( model, structureRequested, Cmd.none )

        StructureFilesSelected first rest ->
            let
                loadedDesigns =
                    List.length rest
                        |> (+) 1
            in
            ( { model | loadingState = Loading loadedDesigns 0 }
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
                        Loading total remaining ->
                            let
                                updatedRemaining =
                                    remaining + 1
                            in
                            if updatedRemaining == total then
                                Free

                            else
                                Loading total updatedRemaining

                        Free ->
                            Free

                rStructuralData =
                    Ampal.parsePdbString name contents
            in
            case rStructuralData of
                Ok structuralData ->
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
                        |> Global.send
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
                |> Global.send
            )

        NoOp ->
            ( model, Cmd.none, Cmd.none )



-- }}}
-- {{{ Cmds


structureRequested : Cmd Msg
structureRequested =
    FileSelect.files [ "*/*" ] StructureFilesSelected



-- }}}


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none


view : Global.Model -> Model -> Element Msg
view globalModel model =
    case globalModel of
        Global.Running runState ->
            column [ spacing 15, width fill ]
                [ row [ centerX, spacing 10 ]
                    [ h1 <| text "Designs"
                    , let
                        ( buttonLabel, isActive ) =
                            case model.loadingState of
                                Loading total remaining ->
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
                , runState.designs
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
designCard ( uuid, designStub ) =
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
            [ Style.conditionalButton
                { labelText = "Details"
                , clickMsg = NoOp
                , isActive = designStub.metricsAvailable
                }
            , Style.dangerousButton
                { labelText = "Delete"
                , confirmText = "Are you sure you want to delete this design?"
                , status = designStub.deleteStatus
                , dangerousMsg = DeleteDesign uuid
                }
            ]
        ]
