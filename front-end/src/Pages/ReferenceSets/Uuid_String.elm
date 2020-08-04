port module Pages.ReferenceSets.Uuid_String exposing (Model, Msg, Params, page)

import Browser.Navigation as Nav
import Codec exposing (Value)
import Dict
import Element exposing (..)
import Shared
import Shared.Buttons as Buttons
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
    ({ uuidString : String, referenceSet : Value } -> msg)
    -> Sub msg



-- }}}
-- {{{ MODEL


type alias Model =
    { key : Nav.Key
    , pageState : PageState
    }


type PageState
    = LoadingNoStub String
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
                    { key = shared.key, pageState = LoadingWithStub params.uuid stub }

                Nothing ->
                    { key = shared.key, pageState = LoadingNoStub params.uuid }
            , ReferenceSet.getStoredReferenceSet { uuidString = params.uuid }
            )

        Nothing ->
            Debug.todo "Should this be dealt with here?"



-- }}}
-- {{{ UPDATE


type Msg
    = SetFocus { uuidString : String, referenceSet : Value }
    | DeleteFocussedReferenceSet String Buttons.DangerStatus


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetFocus { uuidString, referenceSet } ->
            case Codec.decodeValue ReferenceSet.codec referenceSet of
                Ok refSet ->
                    ( { model | pageState = RefSet uuidString refSet }, Cmd.none )

                Err _ ->
                    ( { model | pageState = RefSetNotFound uuidString }, Cmd.none )

        DeleteFocussedReferenceSet uuid dangerStatus ->
            if Buttons.isConfirmed dangerStatus then
                ( { model | pageState = Deleted uuid }
                , Cmd.batch
                    [ ReferenceSet.deleteReferenceSet { uuidString = uuid }
                    , navigate model.key Route.ReferenceSets
                    ]
                )

            else
                ( { model
                    | pageState =
                        mapPageState
                            (\{ uuidString, referenceSet } ->
                                { uuidString = uuidString
                                , referenceSet =
                                    ReferenceSet.updateDeleteStatus dangerStatus referenceSet
                                }
                            )
                            model.pageState
                  }
                , Cmd.none
                )


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
load _ model =
    ( model, Cmd.none )


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
            LoadingNoStub uuidString ->
                el [] ("Loading reference set (id: " ++ uuidString ++ ")..." |> text)

            LoadingWithStub uuidString stub ->
                sectionColumn
                    [ simpleDetails uuidString (stub |> ReferenceSet.getParamsForStub)
                    ]

            RefSetNotFound referenceSetUuid ->
                el [] ("A reference set with ID \"" ++ referenceSetUuid ++ "\" was not found." |> text)

            RefSet uuidString referenceSet ->
                sectionColumn
                    [ simpleDetails uuidString (referenceSet |> ReferenceSet.getGenericData)
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
                    , route = Route.ReferenceSets
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
            ]
        ]



-- }}}
