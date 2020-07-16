module Shared exposing
    ( Flags
    , Model
    , Msg
    , getRunState
    , init
    , mapRunState
    , subscriptions
    , update
    , view
    )

import Browser.Navigation exposing (Key)
import Codec exposing (Codec, Value)
import Design
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Region as Region
import FeatherIcons
import Random
import Shared.ResourceUuid as ResourceUuid exposing (ResourceUuid)
import Shared.Style as Style
import Spa.Document exposing (Document)
import Spa.Generated.Route as Route exposing (Route)
import Url exposing (Url)
import Uuid exposing (Uuid)
import WebSockets as WS



-- {{{ SHARED MODEL


type alias Model =
    { url : Url
    , key : Key
    , appState : AppState
    }


type AppState
    = Running RunState
    | FailedToLaunch LaunchError


type alias RunState =
    { resourceUuid : ResourceUuid
    , webSocketConnectionStatus : WS.ConnectionStatus
    , designs : Dict String Design.StoredDesign

    -- , referenceSets : Dict String StoredReferenceSet
    -- , mSelectedReferenceSet : Maybe String
    -- , specifications : Dict String StoredSpecification
    -- , mSelectedSpecification : Maybe String
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



-- }}}
-- {{{ INIT


type alias Flags =
    Value


type alias InitialData =
    { initialSeed : Int

    -- , mInitialState :
    --     Maybe
    --         { designs : Dict String StoredDesign
    --         , referenceSets : Dict String StoredReferenceSet
    --         , mSelectedReferenceSet : Maybe String
    --         , specifications : Dict String StoredSpecification
    --         , mSelectedSpecification : Maybe String
    --         }
    }


flagsCodec : Codec InitialData
flagsCodec =
    Codec.object InitialData
        |> Codec.field "initialSeed" .initialSeed Codec.int
        -- |> Codec.field "mInitialState"
        --     .mInitialState
        --     (storedStateCodec |> Codec.maybe)
        |> Codec.buildObject


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    ( case Codec.decodeValue flagsCodec flags of
        Ok { initialSeed } ->
            let
                resourceUuid =
                    ResourceUuid.createInitialUuid initialSeed

                mInitialState =
                    Nothing
            in
            case mInitialState of
                Just initialState ->
                    Debug.todo "Handle initial state."

                Nothing ->
                    { url = url
                    , key = key
                    , appState =
                        Running
                            { resourceUuid = resourceUuid
                            , webSocketConnectionStatus = WS.unknownStatus
                            , designs = Dict.empty
                            }

                    -- , designs = Dict.empty
                    -- , referenceSets = Dict.empty
                    -- , mSelectedReferenceSet = Nothing
                    -- , specifications = Dict.empty
                    -- , mSelectedSpecification = Nothing
                    }

        Err codecError ->
            { url = url
            , key = key
            , appState =
                FailedToDecodeFlags codecError |> FailedToLaunch
            }
    , Cmd.none
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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- }}}
-- {{{ VIEW


view :
    { page : Document msg, toMsg : Msg -> msg }
    -> Model
    -> Document msg
view { page } model =
    { title = page.title
    , body =
        [ column
            [ height fill
            , width fill
            , Font.family
                [ Font.typeface "Roboto"
                , Font.sansSerif
                ]
            ]
            [ case model.appState of
                FailedToLaunch _ ->
                    Route.fromUrl model.url
                        |> viewHeader WS.unknownStatus

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


viewHeader : WS.ConnectionStatus -> Maybe Route -> Element msg
viewHeader connStat currentRoute =
    column
        [ padding 10
        , spacing 5
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
                WS.statusIconView connStat
            ]
        , wrappedRow
            [ centerX
            , spacing 10
            , Font.medium
            , Font.size 24
            ]
            ([ viewLink currentRoute ( text "Designs", Route.Designs )
             , viewLink currentRoute ( text "Reference Sets", Route.ReferenceSets )
             , viewLink currentRoute ( text "Specifications", Route.Specifications )
             , viewLink currentRoute
                ( FeatherIcons.settings
                    |> Style.featherIconToElmUi
                , Route.NotFound
                )
             ]
                |> List.intersperse (el [ Font.color Style.colorPalette.c4 ] <| text "|")
            )
        ]


viewLink : Maybe Route -> ( Element msg, Route ) -> Element msg
viewLink mRoute ( label, route ) =
    let
        notHighlightedView : Element msg
        notHighlightedView =
            link
                [ alpha 0.5
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
