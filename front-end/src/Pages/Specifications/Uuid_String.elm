port module Pages.Specifications.Uuid_String exposing (Model, Msg, Params, page)

import Codec exposing (Value)
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import FeatherIcons
import Shared
import Shared.Buttons as Buttons
import Shared.Requirement as Requirement exposing (Requirement, RequirementData)
import Shared.Specification as Specification
    exposing
        ( Specification
        , SpecificationStub
        )
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



-- {{{ PORTS


port setFocussedSpecification :
    ({ uuidString : String, specification : Value } -> msg)
    -> Sub msg



-- }}}
-- {{{ MODEL


type Model
    = LoadingNoStub String
    | LoadingWithStub String SpecificationStub
    | SpecNotFound String
    | Spec String Specification
    | Deleted String


mapModel :
    ({ uuidString : String, specification : Specification }
     -> { uuidString : String, specification : Specification }
    )
    -> Model
    -> Model
mapModel specFn focus =
    case focus of
        LoadingNoStub _ ->
            focus

        LoadingWithStub _ _ ->
            focus

        SpecNotFound _ ->
            focus

        Spec uuidString specification ->
            { uuidString = uuidString, specification = specification }
                |> specFn
                |> (\idSpec -> Spec idSpec.uuidString idSpec.specification)

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
                Dict.get params.uuid runState.specifications
                    |> Maybe.map Specification.storedSpecificationToStub
              of
                Just stub ->
                    LoadingWithStub params.uuid stub

                Nothing ->
                    LoadingNoStub params.uuid
            , Specification.getStoredSpecification { uuidString = params.uuid }
            )

        Nothing ->
            Debug.todo "Should this be dealt with here?"



-- }}}
-- {{{ UPDATE


type Msg
    = SetFocus { uuidString : String, specification : Value }
    | DeleteFocussedSpecification String Buttons.DangerStatus


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetFocus { uuidString, specification } ->
            case Codec.decodeValue Specification.codec specification of
                Ok spec ->
                    ( Spec uuidString spec, Cmd.none )

                Err _ ->
                    ( SpecNotFound uuidString, Cmd.none )

        DeleteFocussedSpecification uuid dangerStatus ->
            if Buttons.isConfirmed dangerStatus then
                ( Deleted uuid
                , Specification.deleteSpecification { uuidString = uuid }
                )

            else
                ( mapModel
                    (\{ uuidString, specification } ->
                        { uuidString = uuidString
                        , specification =
                            { specification
                                | deleteStatus =
                                    dangerStatus
                            }
                        }
                    )
                    model
                , Cmd.none
                )


save : Model -> Shared.Model -> Shared.Model
save model shared =
    case model of
        Deleted uuidString ->
            Shared.mapRunState
                (\runState ->
                    { runState
                        | specifications =
                            Dict.remove uuidString runState.specifications
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
    setFocussedSpecification SetFocus



-- }}}
-- {{{ VIEW


view : Model -> Document Msg
view model =
    { title = "Specifications.Uuid_String"
    , body = [ bodyView model ]
    }


bodyView : Model -> Element Msg
bodyView model =
    el [ centerX, width (fill |> maximum 900) ] <|
        case model of
            LoadingNoStub uuidString ->
                el [] ("Loading specification (id: " ++ uuidString ++ ")..." |> text)

            LoadingWithStub uuidString stub ->
                sectionColumn
                    [ simpleDetails uuidString stub
                    , el [] ("Loading requirements (id: " ++ uuidString ++ ")..." |> text)
                    ]

            SpecNotFound specificationUuid ->
                el [] ("A specification with ID \"" ++ specificationUuid ++ "\" was not found." |> text)

            Spec uuidString specification ->
                sectionColumn
                    [ simpleDetails uuidString specification
                    , sectionColumn
                        [ Style.h2 <| text "Requirements"
                        , Requirement.requirementView specification.requirements
                        ]
                    ]

            Deleted uuidString ->
                el [] ("This specification (" ++ uuidString ++ ") has been deleted." |> text)


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
simpleDetails uuidString specOrStub =
    sectionColumn
        [ wrappedRow [ spacing 10 ]
            [ paragraph [] [ Style.h1 <| text "Specification Details" ]
            , row [ spacing 10 ]
                [ Buttons.linkButton
                    { label = text "Back"
                    , route = Route.Specifications
                    }
                , Buttons.dangerousButton
                    { label = text "Delete"
                    , confirmText = "Are you sure you want to delete this specification?"
                    , status = specOrStub.deleteStatus
                    , dangerousMsg = DeleteFocussedSpecification uuidString
                    }
                ]
            ]
        , sectionColumn
            [ Style.h2 <| text "Name"
            , text specOrStub.name
            , Style.h2 <|
                text "Description"
            , paragraph
                []
                [ text specOrStub.description ]
            ]
        ]



-- }}}
