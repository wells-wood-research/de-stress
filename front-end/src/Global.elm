port module Global exposing
    ( Flags
    , Model(..)
    , Msg(..)
    , RunState
    , createInitialUuid
    , init
    , send
    , subscriptions
    , update
    , updateUuid
    )

import Codec exposing (Codec, Value)
import Dict exposing (Dict)
import Generated.Route as TopRoute
import Generated.Route.Specifications as SpecRoute
import Json.Decode as JDe
import Random
import Specification exposing (Specification)
import Style
import Task
import Uuid exposing (Uuid)


type alias Flags =
    { randomSeed : Int }


flagsCodec : Codec Flags
flagsCodec =
    Codec.object Flags
        |> Codec.field "randomSeed" .randomSeed Codec.int
        |> Codec.buildObject


type Model
    = Running RunState
    | FailedToLaunch LaunchError


type alias RunState =
    { randomSeed : Random.Seed
    , nextUuid : Uuid
    , specifications : Dict String Specification
    }


type LaunchError
    = FailedToDecodeFlags Codec.Error


type Msg
    = AddSpecification Specification
    | DeleteSpecification String Style.DangerStatus
    | RequestedNewUuid



-- {{{ Init


init : { navigate : TopRoute.Route -> Cmd msg } -> JDe.Value -> ( Model, Cmd Msg, Cmd msg )
init _ flagsValue =
    ( case Codec.decodeValue flagsCodec flagsValue of
        Ok flags ->
            let
                ( nextUuid, randomSeed ) =
                    createInitialUuid flags.randomSeed
            in
            Running
                { randomSeed = randomSeed
                , nextUuid = nextUuid
                , specifications = Dict.empty
                }

        Err errString ->
            FailedToDecodeFlags errString |> FailedToLaunch
    , Cmd.none
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
-- {{{ Update


update : { navigate : TopRoute.Route -> Cmd msg } -> Msg -> Model -> ( Model, Cmd Msg, Cmd msg )
update { navigate } msg model =
    case model of
        Running runState ->
            (case msg of
                AddSpecification spec ->
                    let
                        uuidString =
                            Uuid.toString runState.nextUuid
                    in
                    ( { runState
                        | specifications =
                            Dict.insert
                                uuidString
                                spec
                                runState.specifications
                      }
                        |> updateUuid
                    , encodeSpecificationAndKey
                        { storeKey = uuidString
                        , specification = spec
                        }
                        |> storeSpecification
                    , navigate <|
                        TopRoute.Specifications (SpecRoute.All ())
                    )

                DeleteSpecification uuidString dangerStatus ->
                    ( case dangerStatus of
                        Style.Confirmed ->
                            { runState
                                | specifications =
                                    Dict.remove uuidString runState.specifications
                            }

                        _ ->
                            { runState
                                | specifications =
                                    Dict.update
                                        uuidString
                                        (Maybe.map
                                            (\s ->
                                                { s
                                                    | deleteStatus =
                                                        dangerStatus
                                                }
                                            )
                                        )
                                        runState.specifications
                            }
                    , Cmd.none
                    , Cmd.none
                    )

                RequestedNewUuid ->
                    ( runState, Cmd.none, Cmd.none )
            )
                |> asModel Running

        FailedToLaunch launchError ->
            case msg of
                _ ->
                    ( model, Cmd.none, Cmd.none )


asModel : (a -> Model) -> ( a, Cmd Msg, Cmd msg ) -> ( Model, Cmd Msg, Cmd msg )
asModel constructor ( state, gCmds, pCmds ) =
    ( constructor state, gCmds, pCmds )


updateUuid : RunState -> RunState
updateUuid runState =
    let
        ( nextUuid, newSeed ) =
            Random.step Uuid.uuidGenerator runState.randomSeed
    in
    { runState | randomSeed = newSeed, nextUuid = nextUuid }



-- }}}
-- {{{ Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- }}}
-- {{{ Ports


port storeSpecification : Value -> Cmd msg


type alias SpecificationAndKey =
    { storeKey : String
    , specification : Specification
    }


encodeSpecificationAndKey : SpecificationAndKey -> Value
encodeSpecificationAndKey specificationAndKey =
    Codec.encoder
        specificationAndKeyCodec
        specificationAndKey


specificationAndKeyCodec : Codec SpecificationAndKey
specificationAndKeyCodec =
    Codec.object SpecificationAndKey
        |> Codec.field "storeKey" .storeKey Codec.string
        |> Codec.field "specification" .specification Specification.specificationCodec
        |> Codec.buildObject



-- }}}
-- {{{ Utilities


send : msg -> Cmd msg
send =
    Task.succeed >> Task.perform identity



-- }}}
