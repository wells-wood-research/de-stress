module Pages.Specifications exposing (Model, Msg, page)

import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Generated.Params as Params
import Generated.Routes as Routes exposing (routes)
import Global
import Spa.Page exposing (send)
import Specification exposing (SpecificationStub)
import Style exposing (h1)
import Utils.Spa exposing (Page)


page : Page Params.Specifications Model Msg model msg appMsg
page =
    Spa.Page.component
        { title = always "Specifications"
        , init = always init
        , update = always update
        , subscriptions = always subscriptions
        , view = view
        }



-- {{{ Init


type alias Model =
    {}


init : Params.Specifications -> ( Model, Cmd Msg, Cmd Global.Msg )
init _ =
    ( {}
    , Cmd.none
    , Cmd.none
    )



-- }}}
-- {{{ Update


type Msg
    = ClickedSelectSpecification (Maybe String)
    | DeleteSpecification String Style.DangerStatus


update : Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update msg model =
    case msg of
        ClickedSelectSpecification mSelectedSpecification ->
            ( model
            , Cmd.none
            , Global.SetMSelectedSpecification mSelectedSpecification
                |> send
            )

        DeleteSpecification uuidString dangerStatus ->
            ( model
            , Cmd.none
            , Global.DeleteSpecification uuidString dangerStatus
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
        Global.Running { mSelectedSpecification, specifications } ->
            el [ centerX, width <| maximum 800 <| fill ] <|
                column
                    [ width fill, spacing 30 ]
                    (row [ centerX, spacing 10 ]
                        [ Style.h1 <|
                            text "Requirement Specifications"
                        , Style.linkButton { url = "/specifications/new", label = text "New" }
                        ]
                        :: (Dict.toList specifications
                                |> List.map
                                    (\( k, v ) ->
                                        ( k, Global.storedSpecificationToStub v )
                                    )
                                |> List.map (specificationStubView mSelectedSpecification)
                           )
                    )

        Global.FailedToLaunch _ ->
            Debug.todo "Add common state page"


specificationStubView : Maybe String -> ( String, SpecificationStub ) -> Element Msg
specificationStubView mSelectedSpecification ( uuidString, { name, description, deleteStatus } ) =
    column
        ([ padding 15
         , spacing 10
         , width fill
         , Background.color Style.colorPalette.c5
         , Border.rounded 10
         , Border.width 3
         ]
            ++ (case mSelectedSpecification of
                    Just selectedSpecification ->
                        if selectedSpecification == uuidString then
                            [ Border.color Style.colorPalette.black
                            , Events.onClick <| ClickedSelectSpecification <| Nothing
                            ]

                        else
                            [ Border.color Style.colorPalette.c5
                            , Events.onClick <|
                                ClickedSelectSpecification <|
                                    Just uuidString
                            ]

                    Nothing ->
                        [ Border.color Style.colorPalette.c5
                        , Events.onClick <|
                            ClickedSelectSpecification <|
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
                { label = text "Details"
                , url = Routes.toPath <| routes.specifications_dynamic uuidString
                }
            , Style.dangerousButton
                { label = text "Delete"
                , confirmText = "Are you sure you want to delete this specification?"
                , status = deleteStatus
                , dangerousMsg = DeleteSpecification uuidString
                }
            ]
        ]



-- }}}
