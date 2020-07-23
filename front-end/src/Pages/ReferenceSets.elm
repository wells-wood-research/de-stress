module Pages.ReferenceSets exposing (Model, Msg, Params, page)

import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Shared
import Shared.Buttons as Buttons
import Shared.ReferenceSet as ReferenceSet exposing (ReferenceSetStub)
import Shared.Style as Style
import Spa.Document exposing (Document)
import Spa.Generated.Route as Route
import Spa.Page as Page exposing (Page)
import Spa.Url as Url exposing (Url)


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
    { referenceSets : Dict String ReferenceSet.StoredReferenceSet
    , mSelectedReferenceSet : Maybe String
    }



-- }}}
-- {{{ INIT


type alias Params =
    ()


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared _ =
    case Shared.getRunState shared of
        Just runState ->
            ( { referenceSets = runState.referenceSets
              , mSelectedReferenceSet = runState.mSelectedReferenceSet
              }
            , Cmd.none
            )

        Nothing ->
            ( { referenceSets = Dict.empty
              , mSelectedReferenceSet = Nothing
              }
            , Cmd.none
            )



-- }}}
-- {{{ UPDATE


type Msg
    = ClickedSelectReferenceSet (Maybe String)
    | DeleteReferenceSet String Buttons.DangerStatus


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedSelectReferenceSet mSelectedReferenceSet ->
            ( { model | mSelectedReferenceSet = mSelectedReferenceSet }, Cmd.none )

        DeleteReferenceSet uuidString dangerStatus ->
            if Buttons.isConfirmed dangerStatus then
                ( { model
                    | referenceSets =
                        Dict.remove uuidString model.referenceSets
                  }
                , ReferenceSet.deleteReferenceSet { uuidString = uuidString }
                )

            else
                ( { model
                    | referenceSets =
                        Dict.update
                            uuidString
                            ((\s ->
                                { s
                                    | deleteStatus =
                                        dangerStatus
                                }
                             )
                                |> ReferenceSet.mapStubParams
                                |> ReferenceSet.mapStoredReferenceSet
                                |> Maybe.map
                            )
                            model.referenceSets
                  }
                , Cmd.none
                )


save : Model -> Shared.Model -> Shared.Model
save model shared =
    Shared.mapRunState
        (\runState ->
            { runState
                | referenceSets = model.referenceSets
                , mSelectedReferenceSet = model.mSelectedReferenceSet
            }
        )
        shared


load : Shared.Model -> Model -> ( Model, Cmd Msg )
load shared model =
    case Shared.getRunState shared of
        Just runState ->
            ( { model
                | referenceSets = runState.referenceSets
                , mSelectedReferenceSet = runState.mSelectedReferenceSet
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
    { title = "ReferenceSets"
    , body = [ bodyView model ]
    }


bodyView : Model -> Element Msg
bodyView model =
    el [ centerX, width <| maximum 800 <| fill ] <|
        column
            [ width fill, spacing 30 ]
            [ wrappedRow [ centerX, spacing 10 ]
                [ paragraph []
                    [ Style.h1 <|
                        text "Reference Sets"
                    ]
                , Buttons.linkButton
                    { route = Route.ReferenceSets__New, label = text "New" }
                ]
            , column [ width fill, spacing 15 ]
                (Dict.toList model.referenceSets
                    |> List.map
                        (\( k, v ) ->
                            ( k, ReferenceSet.storedReferenceSetToStub v )
                        )
                    |> List.map
                        (referenceSetStubView model.mSelectedReferenceSet)
                )
            ]


referenceSetStubView : Maybe String -> ( String, ReferenceSetStub ) -> Element Msg
referenceSetStubView mSelectedReferenceSet ( uuidString, stub ) =
    let
        { name, description, deleteStatus } =
            ReferenceSet.getParamsForStub stub
    in
    column
        ([ padding 15
         , spacing 10
         , width fill
         , Background.color Style.colorPalette.c5
         , Border.rounded 10
         , Border.width 3
         ]
            ++ (case mSelectedReferenceSet of
                    Just selectedReferenceSet ->
                        if selectedReferenceSet == uuidString then
                            [ Border.color Style.colorPalette.black
                            ]

                        else
                            [ Border.color Style.colorPalette.c5
                            ]

                    Nothing ->
                        [ Border.color Style.colorPalette.c5
                        ]
               )
        )
        [ column
            [ pointer
            , spacing 10
            , width fill
            ]
            [ Style.h2 <| text name
            , paragraph [] [ text description ]
            ]
        , row [ spacing 10, width fill ]
            [ case mSelectedReferenceSet of
                Just selectedReferenceSet ->
                    if selectedReferenceSet == uuidString then
                        Buttons.alwaysActiveButton
                            { label = text "Active"
                            , clickMsg =
                                ClickedSelectReferenceSet <|
                                    Nothing
                            , pressed = True
                            }

                    else
                        Buttons.alwaysActiveButton
                            { label = text "Set Active"
                            , clickMsg =
                                ClickedSelectReferenceSet <|
                                    Just uuidString
                            , pressed = False
                            }

                Nothing ->
                    Buttons.alwaysActiveButton
                        { label = text "Set Active"
                        , clickMsg =
                            ClickedSelectReferenceSet <|
                                Just uuidString
                        , pressed = False
                        }
            , Buttons.linkButton
                { label = text "Details"
                , route = Route.ReferenceSets__Uuid_String { uuid = uuidString }
                }
            , Buttons.dangerousButton
                { label = text "Delete"
                , confirmText = "Are you sure you want to delete this specification?"
                , status = deleteStatus
                , dangerousMsg = DeleteReferenceSet uuidString
                }
            ]
        ]



-- }}}
