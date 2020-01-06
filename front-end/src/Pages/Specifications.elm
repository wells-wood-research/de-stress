module Pages.Specifications exposing (Model, Msg, page)

import Codec exposing (Codec, Value)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Generated.Params as Params
import Generated.Routes as Routes exposing (Route, routes)
import Global
import Ports
import Spa
import Spa.Page exposing (send)
import Specification as Spec exposing (Specification, SpecificationStub)
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
    = DeleteSpecification String Style.DangerStatus


update : Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update msg model =
    case msg of
        DeleteSpecification uuidString dangerStatus ->
            ( model
            , Cmd.none
            , Global.DeleteSpecification uuidString dangerStatus
                |> send
            )



-- }}}
-- {{{ Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- }}}
-- {{{ View


view : Utils.Spa.PageContext -> Model -> Element Msg
view { global } model =
    case global of
        Global.Running { specifications } ->
            column
                [ width fill, spacing 30 ]
                (row [ centerX, spacing 10 ]
                    [ Style.h1 <|
                        text "Requirement Specifications"
                    , Style.linkButton { url = "/specifications/new", labelText = "New" }
                    ]
                    :: (Dict.toList specifications
                            |> List.map
                                (\( k, v ) ->
                                    ( k, Global.storedSpecificationToStub v )
                                )
                            |> List.map specificationStubView
                       )
                )

        Global.FailedToLaunch _ ->
            Debug.todo "Add common state page"


specificationStubView : ( String, SpecificationStub ) -> Element Msg
specificationStubView ( uuidString, { name, description, deleteStatus } ) =
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
                , url = Routes.toPath <| routes.specifications_dynamic uuidString
                }
            , Style.dangerousButton
                { labelText = "Delete"
                , confirmText = "Are you sure you want to delete this specification?"
                , status = deleteStatus
                , dangerousMsg = DeleteSpecification uuidString
                }
            ]
        ]



-- }}}
