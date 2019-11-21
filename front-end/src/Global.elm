module Global exposing
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

import Codec exposing (Codec)
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
    , specifications : List Specification
    }


type LaunchError
    = FailedToDecodeFlags Codec.Error


type Msg
    = AddSpecification Specification
    | DeleteSpecification Int Style.DangerStatus
    | RequestedNewUuid



-- {{{ Init


init : { navigate : TopRoute.Route -> Cmd msg } -> JDe.Value -> ( Model, Cmd Msg, Cmd msg )
init _ flagsValue =
    ( case Codec.decodeValue flagsCodec flagsValue of
        Ok flags ->
            let
                ( nextUuid, initialRandomSeed ) =
                    createInitialUuid flags.randomSeed
            in
            Running
                { randomSeed = initialRandomSeed
                , nextUuid = nextUuid
                , specifications = []
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
                    ( { runState | specifications = spec :: runState.specifications }
                    , Cmd.none
                    , navigate <|
                        TopRoute.Specifications (SpecRoute.All ())
                    )

                DeleteSpecification index dangerStatus ->
                    ( case dangerStatus of
                        Style.Confirmed ->
                            { runState
                                | specifications =
                                    List.indexedMap Tuple.pair runState.specifications
                                        |> List.filter
                                            (\( i, _ ) -> i /= index)
                                        |> List.map Tuple.second
                            }

                        _ ->
                            { runState
                                | specifications =
                                    List.indexedMap
                                        (\i s ->
                                            if i == index then
                                                { s
                                                    | deleteStatus =
                                                        dangerStatus
                                                }

                                            else
                                                s
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


updateUuid : { randomSeed : Random.Seed, nextUuid : Uuid.Uuid } -> { randomSeed : Random.Seed, nextUuid : Uuid.Uuid }
updateUuid { randomSeed } =
    let
        ( nextUuid, newSeed ) =
            Random.step Uuid.uuidGenerator randomSeed
    in
    { randomSeed = newSeed, nextUuid = nextUuid }



-- }}}
-- {{{ Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- }}}
-- {{{ Utilities


send : msg -> Cmd msg
send =
    Task.succeed >> Task.perform identity



-- }}}
