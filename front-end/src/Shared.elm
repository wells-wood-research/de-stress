port module Shared exposing
    ( Flags
    , Model
    , Msg
    , RunState
    , encodeStoredRunState
    , getRunState
    , init
    , mapRunState
    , storeRunState
    , subscriptions
    , update
    , view
    )

import Browser.Events
import Browser.Navigation exposing (Key)
import Codec exposing (Codec, Value)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Region as Region
import FeatherIcons
import Html.Attributes as HAtt
import Json.Decode as JDe
import Shared.Design as Design
import Shared.Error as Error exposing (Error)
import Shared.ReferenceSet as ReferenceSet
import Shared.ResourceUuid as ResourceUuid exposing (ResourceUuid)
import Shared.Specification as Specification
import Shared.Style as Style
import Shared.WebSockets as WebSockets exposing (ConnectionStatus(..))
import Spa.Document exposing (Document)
import Spa.Generated.Route as Route exposing (Route)
import Url exposing (Url)



-- {{{ PORTS


port storeRunState : Value -> Cmd msg


port setWebSocketConnectionStatus : (Value -> msg) -> Sub msg



-- }}}
-- {{{ SHARED MODEL


type alias Model =
    { url : Url
    , key : Key
    , errors : List Error
    , height : Int
    , width : Int
    , appState : AppState
    }


type AppState
    = Running RunState
    | FailedToLaunch LaunchError


type alias RunState =
    { resourceUuid : ResourceUuid
    , webSocketConnectionStatus : ConnectionStatus
    , designs : Dict String Design.StoredDesign
    , referenceSets : Dict String ReferenceSet.StoredReferenceSet
    , mSelectedReferenceSet : Maybe String
    , specifications : Dict String Specification.StoredSpecification
    , mSelectedSpecification : Maybe String
    , saveStateRequested : Bool
    }


getRunState : Model -> Maybe RunState
getRunState model =
    case model.appState of
        Running runState ->
            Just runState

        _ ->
            Nothing


mapRunState : (RunState -> RunState) -> Model -> Model
mapRunState fn model =
    case model.appState of
        Running runState ->
            { model
                | appState = fn runState |> Running
            }

        _ ->
            model


type LaunchError
    = FailedToDecodeFlags Codec.Error


type alias StoredRunState =
    { designs : Dict String Design.StoredDesign
    , referenceSets : Dict String ReferenceSet.StoredReferenceSet
    , mSelectedReferenceSet : Maybe String
    , specifications : Dict String Specification.StoredSpecification
    , mSelectedSpecification : Maybe String
    }


storedRunStateCodec : Codec StoredRunState
storedRunStateCodec =
    Codec.object StoredRunState
        |> Codec.field "designs" .designs (Codec.dict Design.storedDesignCodec)
        |> Codec.field "referenceSets"
            .referenceSets
            (Codec.dict ReferenceSet.storedReferenceSetCodec)
        |> Codec.field "mSelectedReferenceSet"
            .mSelectedReferenceSet
            (Codec.string
                |> Codec.maybe
            )
        |> Codec.field "specifications"
            .specifications
            (Codec.dict Specification.storedSpecificationCodec)
        |> Codec.field "mSelectedSpecification"
            .mSelectedSpecification
            (Codec.string
                |> Codec.maybe
            )
        |> Codec.buildObject


encodeStoredRunState : StoredRunState -> Value
encodeStoredRunState storedRunState =
    Codec.encoder storedRunStateCodec storedRunState



-- }}}
-- {{{ INIT


type alias Flags =
    Value


type alias InitialData =
    { initialSeed : Int
    , mInitialState :
        Maybe
            { designs : Dict String Design.StoredDesign
            , specifications : Dict String Specification.StoredSpecification
            , mSelectedSpecification : Maybe String
            , referenceSets : Dict String ReferenceSet.StoredReferenceSet
            , mSelectedReferenceSet : Maybe String
            }
    , windowHeight : Int
    , windowWidth : Int
    }


flagsCodec : Codec InitialData
flagsCodec =
    Codec.object InitialData
        |> Codec.field "initialSeed" .initialSeed Codec.int
        |> Codec.field "mInitialState"
            .mInitialState
            (storedRunStateCodec |> Codec.maybe)
        |> Codec.field "windowHeight" .windowHeight Codec.int
        |> Codec.field "windowWidth" .windowWidth Codec.int
        |> Codec.buildObject


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    ( case Codec.decodeValue flagsCodec flags of
        Ok { initialSeed, mInitialState, windowHeight, windowWidth } ->
            let
                resourceUuid =
                    ResourceUuid.createInitialUuid initialSeed
            in
            case mInitialState of
                Just initialState ->
                    { url = url
                    , key = key
                    , errors = []
                    , width = windowWidth
                    , height = windowHeight
                    , appState =
                        Running
                            { resourceUuid = resourceUuid
                            , webSocketConnectionStatus = WebSockets.unknownStatus
                            , designs = initialState.designs
                            , referenceSets = initialState.referenceSets
                            , mSelectedReferenceSet =
                                initialState.mSelectedReferenceSet
                            , specifications = initialState.specifications
                            , mSelectedSpecification =
                                initialState.mSelectedSpecification
                            , saveStateRequested = False
                            }
                    }

                Nothing ->
                    { url = url
                    , key = key
                    , errors = []
                    , width = windowWidth
                    , height = windowHeight
                    , appState =
                        Running
                            { resourceUuid = resourceUuid
                            , webSocketConnectionStatus = WebSockets.unknownStatus
                            , designs = Dict.empty
                            , referenceSets = Dict.empty
                            , mSelectedReferenceSet = Nothing
                            , specifications = Dict.empty
                            , mSelectedSpecification = Nothing
                            , saveStateRequested = False
                            }
                    }

        Err codecError ->
            { url = url
            , key = key
            , errors = []
            , width = 0
            , height = 0
            , appState =
                FailedToDecodeFlags codecError |> FailedToLaunch
            }
    , Cmd.none
    )



-- }}}
-- {{{ UPDATE


type Msg
    = SetWebSocketConnectionStatus Value
    | WebSocketIncoming Value
    | DismissError Int
    | PageDimensionsChanged Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetWebSocketConnectionStatus value ->
            let
                ( updatedConnectionStatus, mError ) =
                    case Codec.decodeValue Codec.bool value of
                        Ok bool ->
                            if bool then
                                ( Connected, Nothing )

                            else
                                ( Disconnected, Nothing )

                        Err errorValue ->
                            ( Unknown
                            , { title = "Could not determine server connection status"
                              , details = errorValue |> JDe.errorToString
                              , severity = Error.Low
                              }
                                |> Just
                            )

                updatedModel =
                    mapRunState
                        (\runState ->
                            { runState
                                | webSocketConnectionStatus =
                                    updatedConnectionStatus
                            }
                        )
                        model
            in
            ( { updatedModel
                | errors =
                    case mError of
                        Just error ->
                            error :: updatedModel.errors

                        Nothing ->
                            updatedModel.errors
              }
            , Cmd.none
            )

        WebSocketIncoming value ->
            let
                updateStoredDesignStatus :
                    WebSockets.MetricsServerJobStatus
                    -> Design.StoredDesign
                    -> Design.StoredDesign
                updateStoredDesignStatus status storedDesign =
                    Design.mapStoredDesign
                        (\stub ->
                            { stub
                                | metricsJobStatus =
                                    status
                            }
                        )
                        storedDesign
            in
            case Codec.decodeValue WebSockets.incomingCodec value of
                Ok (WebSockets.ReceivedMetricsJob { uuid, status }) ->
                    ( mapRunState
                        (\runState ->
                            { runState
                                | designs =
                                    Dict.update
                                        uuid
                                        (updateStoredDesignStatus status
                                            |> Maybe.map
                                        )
                                        runState.designs
                            }
                        )
                        model
                    , Design.updateDesignMetricsStatus
                        { uuidString = uuid
                        , updatedMetricsStatus =
                            Codec.encodeToValue
                                WebSockets.metricsServerJobStatusCodec
                                status
                        }
                    )

                Ok WebSockets.CommunicationError ->
                    ( { model
                        | errors =
                            { title = "Error when connecting to server"
                            , details =
                                """An error occurred when communicating with the server.
                                Please try refreshing your browser and check your
                                connection to the internet. If this problem persists,
                                please submit a bug report, see the home page for
                                details on how to do that.
                                """
                            , severity = Error.Medium
                            }
                                :: model.errors
                      }
                    , Cmd.none
                    )

                Err errString ->
                    ( { model
                        | errors =
                            { title = "Unexpected response from server"
                            , details =
                                """The server sent a response that I did not expect.
                                Please try refreshing your browser. If this problem
                                persists, please submit a bug report, see the home page
                                for details on how to do that. Please paste the
                                following information into the bug report:


                                """ ++ JDe.errorToString errString
                            , severity = Error.Medium
                            }
                                :: model.errors
                      }
                    , Cmd.none
                    )

        DismissError index ->
            ( { model
                | errors =
                    List.indexedMap Tuple.pair model.errors
                        |> List.filter (\( i, _ ) -> i /= index)
                        |> List.map Tuple.second
              }
            , Cmd.none
            )

        PageDimensionsChanged width height ->
            ( { model
                | width = width
                , height = height
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ setWebSocketConnectionStatus SetWebSocketConnectionStatus
        , WebSockets.incoming WebSocketIncoming
        , Browser.Events.onResize PageDimensionsChanged
        ]



-- }}}
-- {{{ VIEW


view :
    { page : Document msg, toMsg : Msg -> msg }
    -> Model
    -> Document msg
view { page, toMsg } model =
    { title = page.title
    , body =
        [ column
            [ height fill
            , width fill
            , Font.family
                [ Font.typeface "Roboto"
                , Font.sansSerif
                ]
            , allErrorsView
                toMsg
                model.errors
                |> inFront
            ]
            [ case model.appState of
                FailedToLaunch _ ->
                    Route.fromUrl model.url
                        |> viewHeader WebSockets.unknownStatus

                Running { webSocketConnectionStatus } ->
                    Route.fromUrl model.url
                        |> viewHeader webSocketConnectionStatus
            , column
                [ centerX
                , paddingXY 50 30
                , spacing 30
                , width fill
                ]
                page.body
            ]
        ]
    }


allErrorsView : (Msg -> msg) -> List Error -> Element msg
allErrorsView toMsg errors =
    column
        [ padding 10
        , spacing 10
        , htmlAttribute <| HAtt.style "position" "fixed"
        ]
        (List.indexedMap (errorView toMsg) errors)


errorView : (Msg -> msg) -> Int -> Error -> Element msg
errorView toMsg index { title, details } =
    column
        [ padding 10
        , spacing 10
        , width <| px 300
        , Background.color Style.colorPalette.c2
        , Border.rounded 5
        , Font.size 16
        ]
        [ row
            [ width fill ]
            [ paragraph [ Font.bold ] [ text title ]
            , FeatherIcons.x
                |> Style.featherIconToElmUi
                |> el
                    [ alignRight
                    , pointer
                    , Events.onClick <| toMsg <| DismissError index
                    ]
            ]
        , paragraph [] [ text details ]
        ]


viewHeader : ConnectionStatus -> Maybe Route -> Element msg
viewHeader connStat currentRoute =
    column
        [ padding 10
        , width fill
        , Background.color Style.colorPalette.c1
        , Font.color Style.colorPalette.c4
        , Font.size 32
        , Font.bold
        , Region.navigation
        ]
        [ row [ centerX, spacing 10 ]
            [ Style.h1 <| link [] { url = "/", label = text "DE-STRESS" }
            , el [] <|
                WebSockets.statusIconView connStat
            ]
        , row
            [ centerX
            , paddingEach { top = 0, right = 0, bottom = 12, left = 0 }
            , scrollbarX
            , spacing 10
            , width <|
                maximum 500 <|
                    fill
            , Font.medium
            , Font.size 24
            ]
            ([ viewLink currentRoute ( text "Designs", Route.Designs )
             , viewLink currentRoute ( text "Reference Sets", Route.ReferenceSets )
             , viewLink currentRoute ( text "Specifications", Route.Specifications )
             ]
                |> List.intersperse (el [ centerY, Font.color Style.colorPalette.c4 ] <| text "|")
            )
        ]


viewLink : Maybe Route -> ( Element msg, Route ) -> Element msg
viewLink mRoute ( label, route ) =
    let
        notHighlightedView : Element msg
        notHighlightedView =
            link
                [ alpha 0.5
                , centerY
                , mouseOver [ alpha 1 ]
                ]
                { label = label
                , url = Route.toString route
                }
    in
    case mRoute of
        Just currentRoute ->
            if currentRoute == route then
                el
                    []
                    label

            else
                notHighlightedView

        Nothing ->
            notHighlightedView



-- }}}
