module Pages.Designs.Overview exposing (Model, Msg, Params, page)

import Dict
import Element exposing (..)
import Element.Border as Border
import Element.Keyed as Keyed
import Html
import Html.Attributes as HAtt
import Json.Decode as JDe
import Shared
import Shared.Design as Design exposing (DesignStub)
import Shared.Metrics as Metrics
import Shared.Plots as Plots
import Shared.WebSockets as WebSockets
import Spa.Document exposing (Document)
import Spa.Page as Page exposing (Page)
import Spa.Url as Url exposing (Url)
import VegaLite as VL


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
    { designStubs : List DesignStub }



-- }}}
-- {{{ INIT


type alias Params =
    ()


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared _ =
    let
        model =
            { designStubs =
                case Shared.getRunState shared of
                    Just runState ->
                        Dict.values runState.designs
                            |> List.map Design.storedDesignToStub

                    Nothing ->
                        []
            }
    in
    ( model
    , model.designStubs
        |> makeOverViewSpec
        |> Plots.vegaPlot
    )



-- }}}
-- {{{ UPDATE


type Msg
    = ReplaceMe


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReplaceMe ->
            ( model, Cmd.none )


save : Model -> Shared.Model -> Shared.Model
save _ shared =
    shared


load : Shared.Model -> Model -> ( Model, Cmd Msg )
load shared model =
    let
        updatedModel =
            { model
                | designStubs =
                    case Shared.getRunState shared of
                        Just runState ->
                            Dict.values runState.designs
                                |> List.map Design.storedDesignToStub

                        Nothing ->
                            []
            }
    in
    ( updatedModel
    , updatedModel.designStubs
        |> makeOverViewSpec
        |> Plots.vegaPlot
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Designs Overview"
    , body = [ bodyView model ]
    }


bodyView : Model -> Element msg
bodyView _ =
    column
        [ width fill ]
        [ Keyed.el [ centerX ]
            ( "overview"
            , Html.div
                [ HAtt.id "overview"
                , HAtt.style "width" "100%"
                ]
                [ Html.div
                    [ HAtt.height 200
                    , HAtt.style "height" "200px"
                    , HAtt.style "width" "100%"
                    , HAtt.style "border-radius" "5px"
                    , HAtt.style "background-color" "#d3d3d3"
                    ]
                    []
                ]
                |> html
            )
        ]


makeOverViewSpec : List DesignStub -> { plotId : String, spec : VL.Spec }
makeOverViewSpec designStubs =
    { plotId = "overview"
    , spec =
        Metrics.overviewSpec
            "Packing Density"
            (designStubs
                |> List.filterMap
                    (\ds ->
                        case WebSockets.getDesignMetrics ds.metricsJobStatus of
                            Just metrics ->
                                Just ( ds.name, metrics.packingDensity )

                            Nothing ->
                                Nothing
                    )
                |> Dict.fromList
            )
    }
