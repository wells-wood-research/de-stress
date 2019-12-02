port module Pages.Designs exposing
    ( Model
    , Msg
    , page
    )

import Ampal
import Application.Page as Page
import Codec exposing (Value)
import Design exposing (Design)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Keyed as Keyed
import File exposing (File)
import File.Select as FileSelect
import Global
import Html
import Html.Attributes as HAtt
import RemoteData as RD exposing (RemoteData)
import Style exposing (h1, h2)
import Task


type alias Model =
    { loadingState : DesignLoadingState
    , loadErrors : List String
    , focussedDesign : FocussedDesign
    }


type FocussedDesign
    = NoFocus
    | Loading
    | Design String Design


mapFocussedDesign :
    ({ uuidString : String, design : Design }
     -> { uuidString : String, design : Design }
    )
    -> FocussedDesign
    -> FocussedDesign
mapFocussedDesign designFn focus =
    case focus of
        NoFocus ->
            focus

        Loading ->
            focus

        Design uuidString design ->
            { uuidString = uuidString, design = design }
                |> designFn
                |> (\idDesign -> Design idDesign.uuidString idDesign.design)


type DesignLoadingState
    = LoadingFiles Int Int
    | Free


type Msg
    = StructuresRequested
    | StructureFilesSelected File (List File)
    | StructureLoaded String String
    | DeleteDesign String Style.DangerStatus
    | DeleteFocussedDesign String Style.DangerStatus
    | ClickedFocusDesign String
    | SetFocus Value
    | ClearFocus


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
    ( { loadingState = Free, loadErrors = [], focussedDesign = NoFocus }
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

        ClickedFocusDesign uuidString ->
            ( { model | focussedDesign = Loading }
            , Cmd.none
            , Global.GetDesign uuidString
                |> Global.send
            )

        SetFocus value ->
            let
                focusCodec =
                    Codec.object
                        (\uuidString design ->
                            { uuidString = uuidString
                            , design = design
                            }
                        )
                        |> Codec.field "uuidString" .uuidString Codec.string
                        |> Codec.field "design" .design Design.codec
                        |> Codec.buildObject
            in
            case Codec.decodeValue focusCodec value of
                Ok { uuidString, design } ->
                    ( { model | focussedDesign = Design uuidString design }
                    , Cmd.none
                    , Cmd.none
                    )

                Err errorString ->
                    Debug.todo "Catch this error"

        ClearFocus ->
            ( { model | focussedDesign = NoFocus }
            , Cmd.none
            , Cmd.none
            )

        DeleteDesign uuidString dangerStatus ->
            ( model
            , Cmd.none
            , Global.DeleteDesign uuidString dangerStatus
                |> Global.send
            )

        DeleteFocussedDesign globalUuidString dangerStatus ->
            ( case dangerStatus of
                Style.Confirmed ->
                    { model
                        | focussedDesign =
                            NoFocus
                    }

                _ ->
                    { model
                        | focussedDesign =
                            mapFocussedDesign
                                (\{ uuidString, design } ->
                                    { uuidString = uuidString
                                    , design =
                                        { design
                                            | deleteStatus =
                                                dangerStatus
                                        }
                                    }
                                )
                                model.focussedDesign
                    }
            , Cmd.none
            , Global.DeleteDesign globalUuidString dangerStatus
                |> Global.send
            )



-- }}}
-- {{{ Cmds


structureRequested : Cmd Msg
structureRequested =
    FileSelect.files [ "*/*" ] StructureFilesSelected



-- }}}
-- {{{ Subs


port setFocussedDesign : (Value -> msg) -> Sub msg


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions _ _ =
    Sub.batch
        [ setFocussedDesign SetFocus ]



-- }}}
-- {{{ View


view : Global.Model -> Model -> Element Msg
view globalModel model =
    case globalModel of
        Global.Running { designs } ->
            case model.focussedDesign of
                NoFocus ->
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

                Loading ->
                    el [] (text "Loading...")

                Design uuidString design ->
                    designDetailsView uuidString design

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
            [ Style.alwaysActiveButton
                { labelText = "Details"
                , clickMsg = ClickedFocusDesign uuidString
                }
            , Style.dangerousButton
                { labelText = "Delete"
                , confirmText = "Are you sure you want to delete this design?"
                , status = designStub.deleteStatus
                , dangerousMsg = DeleteDesign uuidString
                }
            ]
        ]



-- {{{ View: Details
---- Design Details View ----


designDetailsView : String -> Design -> Element Msg
designDetailsView uuidString { name, fileName, deleteStatus } =
    let
        sectionColumn =
            column [ spacing 10, width fill ]
    in
    column
        [ spacing 15, width fill ]
        [ sectionColumn
            [ paragraph [ centerX ]
                [ h1 <| text (Design.editableValue name ++ " Design Details") ]
            , paragraph [] [ text ("Structure file: " ++ fileName) ]
            ]
        , row [ spacing 10 ]
            [ Style.alwaysActiveButton
                { labelText = "Back"
                , clickMsg = ClearFocus
                }
            , Style.dangerousButton
                { labelText = "Delete"
                , confirmText = "Are you sure you want to delete this design?"
                , status = deleteStatus
                , dangerousMsg = DeleteFocussedDesign uuidString
                }
            ]
        , sectionColumn
            [ h2 <| text "Structure"
            , Keyed.el [ height <| px 300, width fill ]
                ( "viewer"
                , Html.div
                    [ HAtt.id "viewer"
                    , HAtt.style "height" "100%"
                    , HAtt.style "width" "100%"
                    ]
                    []
                    |> html
                )
            ]
        , sectionColumn
            [ h2 <| text "Sequences"
            ]

        -- , metricsView designMetrics
        -- , compareToPdb designMetrics referenceSetMetrics
        -- , case mSpecification of
        --     Nothing ->
        --         none
        --     Just specification ->
        --         sectionColumn
        --             [ Common.h2 <| text "Active Requirement Specification"
        --             , specificationView sequenceStrings designMetrics specification
        --             ]
        ]



-- }}}
-- }}}
