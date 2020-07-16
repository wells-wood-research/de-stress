module Shared exposing
    ( Flags
    , Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Browser.Navigation exposing (Key)
import Codec exposing (Codec, Value)
import Element exposing (..)
import Element.Font as Font
import Random
import Spa.Document exposing (Document)
import Spa.Generated.Route as Route
import Url exposing (Url)
import Uuid exposing (Uuid)



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
    { randomSeed : Random.Seed
    , nextUuid : Uuid

    -- , webSocketConnectionStatus : WebSocketConnectionStatus
    -- , designs : Dict String StoredDesign
    -- , referenceSets : Dict String StoredReferenceSet
    -- , mSelectedReferenceSet : Maybe String
    -- , specifications : Dict String StoredSpecification
    -- , mSelectedSpecification : Maybe String
    }


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
                ( nextUuid, randomSeed ) =
                    createInitialUuid initialSeed

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
                            { randomSeed = randomSeed
                            , nextUuid =
                                nextUuid
                            }

                    -- , nextUuid = nextUuid
                    -- , webSocketConnectionStatus = Unknown
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


createInitialUuid : Int -> ( Uuid, Random.Seed )
createInitialUuid initialRandomNumber =
    let
        initialRandomSeed =
            Random.initialSeed initialRandomNumber
    in
    Random.step Uuid.uuidGenerator initialRandomSeed



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
subscriptions model =
    Sub.none



-- }}}
-- {{{ VIEW


view :
    { page : Document msg, toMsg : Msg -> msg }
    -> Model
    -> Document msg
view { page, toMsg } model =
    { title = page.title
    , body =
        [ column [ padding 20, spacing 20, height fill ]
            [ row [ spacing 20 ]
                [ link [ Font.color (rgb 0 0.25 0.5), Font.underline ]
                    { url = Route.toString Route.Top, label = text "Homepage" }
                , link [ Font.color (rgb 0 0.25 0.5), Font.underline ]
                    { url = Route.toString Route.NotFound, label = text "Not found" }
                ]
            , column [ height fill ] page.body
            ]
        ]
    }



-- }}}
