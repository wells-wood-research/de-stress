module Pages.ReferenceSets.Dynamic exposing (Model, Msg, page)

import Codec exposing (Value)
import Element exposing (..)
import Generated.ReferenceSets.Params as Params
import Generated.Routes as Routes exposing (routes)
import Global
import Ports
import ReferenceSet exposing (ReferenceSet(..))
import Spa.Page exposing (send)
import Style
import Utils.Spa exposing (Page)


page : Page Params.Dynamic Model Msg model msg appMsg
page =
    Spa.Page.component
        { title = always "Reference Sets Details"
        , init = always init
        , update = always update
        , subscriptions = always subscriptions
        , view = always view
        }



-- {{{ Init


type Model
    = Loading String
    | RefSetNotFound String
    | RefSet String ReferenceSet


mapModel :
    ({ uuidString : String, referenceSet : ReferenceSet }
     -> { uuidString : String, referenceSet : ReferenceSet }
    )
    -> Model
    -> Model
mapModel refSetFn focus =
    case focus of
        Loading _ ->
            focus

        RefSetNotFound _ ->
            focus

        RefSet uuidString referenceSet ->
            { uuidString = uuidString, referenceSet = referenceSet }
                |> refSetFn
                |> (\idRefSet -> RefSet idRefSet.uuidString idRefSet.referenceSet)


init : Params.Dynamic -> ( Model, Cmd Msg, Cmd Global.Msg )
init { param1 } =
    ( Loading param1
    , Cmd.none
    , Global.GetReferenceSet param1
        |> send
    )



-- }}}
-- UPDATE


type Msg
    = SetFocus Value
    | DeleteFocussedReferenceSet String Style.DangerStatus


update : Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update msg model =
    case msg of
        SetFocus value ->
            let
                focusCodec =
                    Codec.object
                        (\uuidString refSet ->
                            { uuidString = uuidString
                            , referenceSet = refSet
                            }
                        )
                        |> Codec.field "uuidString" .uuidString Codec.string
                        |> Codec.field "referenceSet"
                            .referenceSet
                            ReferenceSet.codec
                        |> Codec.buildObject
            in
            case Codec.decodeValue focusCodec value of
                Ok { uuidString, referenceSet } ->
                    ( RefSet uuidString referenceSet
                    , Cmd.none
                    , Cmd.none
                    )

                Err errorString ->
                    Debug.todo "Catch this error"

        DeleteFocussedReferenceSet globalUuidString dangerStatus ->
            case dangerStatus of
                Style.Confirmed ->
                    ( model
                    , Cmd.none
                    , Global.DeleteFocussedReferenceSet globalUuidString dangerStatus
                        |> send
                    )

                _ ->
                    ( mapModel
                        (\{ uuidString, referenceSet } ->
                            { uuidString = uuidString
                            , referenceSet =
                                case referenceSet of
                                    HighResBiolUnit params ->
                                        HighResBiolUnit
                                            { params
                                                | deleteStatus =
                                                    dangerStatus
                                            }

                                    PdbCodeList params ->
                                        PdbCodeList
                                            { params
                                                | deleteStatus =
                                                    dangerStatus
                                            }
                            }
                        )
                        model
                    , Cmd.none
                    , Cmd.none
                    )



-- {{{ Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.setFocussedReferenceSet SetFocus ]



-- }}}
-- {{{ View


view : Model -> Element Msg
view model =
    case model of
        Loading _ ->
            el [] (text "Loading reference set...")

        RefSetNotFound referenceSetUuid ->
            "A reference set with ID \""
                ++ referenceSetUuid
                ++ "\" was not found."
                |> text
                |> el []

        RefSet uuidString referenceSet ->
            referenceSetDetailsView uuidString referenceSet


sectionColumn : List (Element msg) -> Element msg
sectionColumn =
    column [ spacing 12, width fill ]


referenceSetDetailsView : String -> ReferenceSet -> Element Msg
referenceSetDetailsView uuidString referenceSet =
    let
        { name, description, deleteStatus } =
            ReferenceSet.getGenericData referenceSet
    in
    column
        [ spacing 12
        , width fill
        ]
        [ sectionColumn
            [ paragraph [] [ Style.h1 <| text "Reference Set Details" ]
            ]
        , row [ spacing 10 ]
            [ Style.linkButton
                { label = text "Back"
                , url = Routes.toPath routes.referenceSets
                }
            , Style.dangerousButton
                { label = text "Delete"
                , confirmText = "Are you sure you want to delete this specification?"
                , status = deleteStatus
                , dangerousMsg = DeleteFocussedReferenceSet uuidString
                }
            ]
        , sectionColumn
            [ Style.h2 <| text "Name"
            , text name
            , Style.h2 <|
                text "Description"
            , paragraph
                []
                [ text description ]
            ]
        ]



-- }}}
