module Pages.ReferenceSets exposing (Model, Msg, page)

import Components.DropDown as DropDown
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
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
        , init = always init
        , update = always update
        , subscriptions = always subscriptions
        , view = view
        }



-- {{{ Init


type alias Model =
    {}


init : Params.ReferenceSets -> ( Model, Cmd Msg, Cmd Global.Msg )
init _ =
    ( {}
    , Cmd.none
    , Cmd.none
    )



-- }}}
-- {{{ Update


type Msg
    = ClickedSelectReferenceSet (Maybe String)
    | DeleteReferenceSet String Style.DangerStatus


update : Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update msg model =
    case msg of
        ClickedSelectReferenceSet mSelectedReferenceSet ->
            ( model
            , Cmd.none
            , Global.SetSelectedReferenceSet mSelectedReferenceSet
                |> send
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
view { global } _ =
    case global of
        Global.Running { mSelectedReferenceSet, referenceSets } ->
            column
                [ width fill, spacing 30 ]
                [ row [ centerX, spacing 10 ]
                    [ Style.h1 <|
                        text "Reference Sets"
                    , Style.linkButton
                        { url = "/reference-sets/new", labelText = "New" }
                    ]
                , column []
                    (Dict.toList referenceSets
                        |> List.map
                            (\( k, v ) ->
                                ( k, Global.storedReferenceSetToStub v )
                            )
                        |> List.map (referenceSetStubView mSelectedReferenceSet)
                    )
                ]

        Global.FailedToLaunch _ ->
            Debug.todo "Add common state page"


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
                            , Events.onClick <| ClickedSelectReferenceSet <| Nothing
                            ]

                        else
                            [ Border.color Style.colorPalette.c5
                            , Events.onClick <|
                                ClickedSelectReferenceSet <|
                                    Just uuidString
                            ]

                    Nothing ->
                        [ Border.color Style.colorPalette.c5
                        , Events.onClick <|
                            ClickedSelectReferenceSet <|
                                Just uuidString
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
