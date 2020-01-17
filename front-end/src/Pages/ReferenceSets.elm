module Pages.ReferenceSets exposing (Model, Msg, page)

import Components.DropDown as DropDown
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Generated.Params as Params
import Generated.Routes as Routes exposing (routes)
import Global
import ReferenceSet exposing (ReferenceSetStub)
import Spa.Page exposing (send)
import Style exposing (h1)
import Utils.Spa exposing (Page)


page : Page Params.ReferenceSets Model Msg model msg appMsg
page =
    Spa.Page.component
        { title = always "ReferenceSets"
        , init = init
        , update = always update
        , subscriptions = always subscriptions
        , view = view
        }



-- {{{ Init


type alias Model =
    { refSetDropDown : DropDown.Model (Maybe ReferenceSetStub)
    }


init : Utils.Spa.PageContext -> Params.ReferenceSets -> ( Model, Cmd Msg, Cmd Global.Msg )
init { global } _ =
    ( case global of
        Global.Running runState ->
            { refSetDropDown = DropDown.init runState.mSelectedReferenceSet }

        Global.FailedToLaunch _ ->
            { refSetDropDown = DropDown.init Nothing }
    , Cmd.none
    , Cmd.none
    )



-- }}}
-- {{{ Update


type Msg
    = RefSetDropDownMsg (DropDown.Msg (Maybe ReferenceSetStub))
    | DeleteReferenceSet String Style.DangerStatus


update : Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update msg model =
    case msg of
        RefSetDropDownMsg cMsg ->
            let
                cModel =
                    DropDown.update cMsg model.refSetDropDown
            in
            ( { model | refSetDropDown = cModel }
            , Cmd.none
            , Cmd.none
            )

        DeleteReferenceSet uuidString dangerStatus ->
            ( model
            , Cmd.none
            , Global.DeleteReferenceSet uuidString dangerStatus
                |> send
            )



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
        Global.Running { referenceSets } ->
            column
                [ width fill, spacing 30 ]
                [ row [ centerX, spacing 10 ]
                    [ Style.h1 <|
                        text "Reference Sets"
                    , Style.linkButton
                        { url = "/reference-sets/new", labelText = "New" }
                    ]
                , Element.map RefSetDropDownMsg <|
                    DropDown.view
                        (\mStub ->
                            case mStub of
                                Just stub ->
                                    let
                                        { name } =
                                            ReferenceSet.getParamsForStub stub
                                    in
                                    text name

                                Nothing ->
                                    text "--"
                        )
                        (Nothing
                            :: (referenceSets
                                    |> Dict.values
                                    |> List.map Global.storedReferenceSetToStub
                                    |> List.map Just
                               )
                        )
                        model.refSetDropDown
                , column []
                    (Dict.toList referenceSets
                        |> List.map
                            (\( k, v ) ->
                                ( k, Global.storedReferenceSetToStub v )
                            )
                        |> List.map referenceSetStubView
                    )
                ]

        Global.FailedToLaunch _ ->
            Debug.todo "Add common state page"


referenceSetStubView : ( String, ReferenceSetStub ) -> Element Msg
referenceSetStubView ( uuidString, stub ) =
    let
        { name, description, deleteStatus } =
            ReferenceSet.getParamsForStub stub
    in
    column
        [ padding 15
        , spacing 10
        , width fill
        , Background.color Style.colorPalette.c5
        , Border.rounded 10
        ]
        [ column
            [ pointer
            , spacing 10
            , width fill
            ]
            [ Style.h2 <| text name
            , paragraph [] [ text description ]
            ]
        , row [ spacing 10, width fill ]
            [ Style.linkButton
                { labelText = "Details"
                , url = Routes.toPath <| routes.referenceSets_dynamic uuidString
                }
            , Style.dangerousButton
                { labelText = "Delete"
                , confirmText = "Are you sure you want to delete this specification?"
                , status = deleteStatus
                , dangerousMsg = DeleteReferenceSet uuidString
                }
            ]
        ]



-- }}}
