module Global exposing
    ( Flags
    , Model(..)
    , Msg(..)
    , init
    , storedSpecificationToStub
    , subscriptions
    , update
    )

import Codec exposing (Codec, Value)
import Design exposing (Design, DesignStub)
import Dict exposing (Dict)
import Generated.Routes as Routes exposing (Route)
import Ports
import Random
import Specification exposing (Specification, SpecificationStub)
import Style
import Task
import Uuid exposing (Uuid)



-- {{{ Flags


type alias Flags =
    Value


type alias DecodedFlags =
    { randomSeed : Int
    , mInitialState :
        Maybe
            { designs : Dict String StoredDesign
            , specifications : Dict String StoredSpecification
            }
    }


flagsCodec : Codec DecodedFlags
flagsCodec =
    Codec.object DecodedFlags
        |> Codec.field "randomSeed" .randomSeed Codec.int
        |> Codec.field "mInitialState"
            .mInitialState
            (storedStateCodec
                |> Codec.maybe
            )
        |> Codec.buildObject


storedStateCodec :
    Codec
        { designs : Dict String StoredDesign
        , specifications : Dict String StoredSpecification
        }
storedStateCodec =
    Codec.object (\ds ss -> { designs = ds, specifications = ss })
        |> Codec.field "designs" .designs (Codec.dict storedDesignCodec)
        |> Codec.field "specifications" .specifications (Codec.dict storedSpecificationCodec)
        |> Codec.buildObject


type StoredDesign
    = LocalDesign DesignStub


storedDesignCodec : Codec StoredDesign
storedDesignCodec =
    Codec.custom
        (\flocal value ->
            case value of
                LocalDesign stub ->
                    flocal stub
        )
        |> Codec.variant1 "LocalDesign" LocalDesign Design.designStubCodec
        |> Codec.buildCustom


storedDesignToStub : StoredDesign -> DesignStub
storedDesignToStub storedDesign =
    case storedDesign of
        LocalDesign stub ->
            stub


mapStoredDesign : (DesignStub -> DesignStub) -> StoredDesign -> StoredDesign
mapStoredDesign stubFn storedDesign =
    case storedDesign of
        LocalDesign stub ->
            stubFn stub |> LocalDesign


type StoredSpecification
    = LocalSpecification SpecificationStub


storedSpecificationCodec : Codec StoredSpecification
storedSpecificationCodec =
    Codec.custom
        (\flocal value ->
            case value of
                LocalSpecification stub ->
                    flocal stub
        )
        |> Codec.variant1 "LocalSpecification" LocalSpecification Specification.specificationStubCodec
        |> Codec.buildCustom


mapStoredSpecification : (SpecificationStub -> SpecificationStub) -> StoredSpecification -> StoredSpecification
mapStoredSpecification stubFn storedSpecification =
    case storedSpecification of
        LocalSpecification stub ->
            stubFn stub |> LocalSpecification


storedSpecificationToStub : StoredSpecification -> SpecificationStub
storedSpecificationToStub storedSpecification =
    case storedSpecification of
        LocalSpecification stub ->
            stub



-- }}}
-- {{{ Model


type Model
    = Running RunState
    | FailedToLaunch LaunchError


type alias RunState =
    { randomSeed : Random.Seed
    , nextUuid : Uuid
    , designs : Dict String StoredDesign
    , specifications : Dict String StoredSpecification
    }


type LaunchError
    = FailedToDecodeFlags Codec.Error


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
        |> Codec.field "specification" .specification Specification.codec
        |> Codec.buildObject



-- }}}
-- {{{ Init


init : Commands msg -> Value -> ( Model, Cmd Msg, Cmd msg )
init _ flagsValue =
    ( case Codec.decodeValue flagsCodec flagsValue of
        Ok flags ->
            let
                ( nextUuid, randomSeed ) =
                    createInitialUuid flags.randomSeed
            in
            case flags.mInitialState of
                Just initialState ->
                    Running
                        { randomSeed = randomSeed
                        , nextUuid = nextUuid
                        , designs = initialState.designs
                        , specifications = initialState.specifications
                        }

                Nothing ->
                    Running
                        { randomSeed = randomSeed
                        , nextUuid = nextUuid
                        , designs = Dict.empty
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


type Msg
    = AddDesign Design
    | DeleteDesign String Style.DangerStatus
    | GetDesign String
    | AddSpecification Specification
    | DeleteSpecification String Style.DangerStatus
    | GetSpecification String
    | RequestedNewUuid


update : Commands msg -> Msg -> Model -> ( Model, Cmd Msg, Cmd msg )
update _ msg model =
    case model of
        Running runState ->
            updateRunState msg runState
                |> addStoreCmd
                |> asModel Running

        FailedToLaunch launchError ->
            case msg of
                _ ->
                    ( model, Cmd.none, Cmd.none )


asModel : (a -> Model) -> ( a, Cmd Msg, Cmd msg ) -> ( Model, Cmd Msg, Cmd msg )
asModel constructor ( state, gCmd, pCmd ) =
    ( constructor state, gCmd, pCmd )


updateRunState : Msg -> RunState -> ( RunState, Cmd Msg, Cmd msg )
updateRunState msg runState =
    case msg of
        AddDesign design ->
            let
                uuidString =
                    Uuid.toString runState.nextUuid
            in
            ( { runState
                | designs =
                    Dict.insert
                        uuidString
                        (design
                            |> Design.createDesignStub
                            |> LocalDesign
                        )
                        runState.designs
              }
                |> updateUuid
            , Cmd.none
              -- , encodeDesignAndKey
              --     { storeKey = uuidString
              --     , design = design
              --     }
              --     |> storeDesign
            , Cmd.none
            )

        DeleteDesign uuidString dangerStatus ->
            case dangerStatus of
                Style.Confirmed ->
                    ( { runState
                        | designs =
                            Dict.remove uuidString runState.designs
                      }
                    , Cmd.none
                      -- , deleteDesign uuidString
                    , Cmd.none
                    )

                _ ->
                    ( { runState
                        | designs =
                            Dict.update
                                uuidString
                                ((\d ->
                                    { d
                                        | deleteStatus =
                                            dangerStatus
                                    }
                                 )
                                    |> mapStoredDesign
                                    |> Maybe.map
                                )
                                runState.designs
                      }
                    , Cmd.none
                    , Cmd.none
                    )

        GetDesign uuidString ->
            ( runState
            , Cmd.none
              -- , getDesign uuidString
            , Cmd.none
            )

        AddSpecification spec ->
            let
                uuidString =
                    Uuid.toString runState.nextUuid
            in
            ( { runState
                | specifications =
                    Dict.insert
                        uuidString
                        (spec
                            |> Specification.createSpecificationStub
                            |> LocalSpecification
                        )
                        runState.specifications
              }
                |> updateUuid
            , Cmd.none
            , encodeSpecificationAndKey
                { storeKey = uuidString
                , specification = spec
                }
                |> Ports.storeSpecification
              -- , navigate <|
              --     TopRoute.Specifications (SpecRoute.All ())
            )

        GetSpecification uuidString ->
            ( runState
            , Cmd.none
            , Codec.encoder Codec.string uuidString
                |> Ports.getSpecification
            )

        DeleteSpecification uuidString dangerStatus ->
            case dangerStatus of
                Style.Confirmed ->
                    ( { runState
                        | specifications =
                            Dict.remove uuidString runState.specifications
                      }
                    , Cmd.none
                    , Codec.encoder Codec.string uuidString
                        |> Ports.deleteSpecification
                    )

                _ ->
                    ( { runState
                        | specifications =
                            Dict.update
                                uuidString
                                ((\s ->
                                    { s
                                        | deleteStatus =
                                            dangerStatus
                                    }
                                 )
                                    |> mapStoredSpecification
                                    |> Maybe.map
                                )
                                runState.specifications
                      }
                    , Cmd.none
                    , Cmd.none
                    )

        RequestedNewUuid ->
            ( runState, Cmd.none, Cmd.none )


addStoreCmd : ( RunState, Cmd Msg, Cmd msg ) -> ( RunState, Cmd Msg, Cmd msg )
addStoreCmd ( state, gCmd, pCmd ) =
    ( state
    , Cmd.none
      -- , Cmd.batch
      --     [ gCmd
      --     , encodeStoredState
      --         { designs = state.designs
      --         , specifications =
      --             state.specifications
      --         }
      --         |> storeRunState
      --     ]
    , pCmd
    )


updateUuid : RunState -> RunState
updateUuid runState =
    let
        ( nextUuid, newSeed ) =
            Random.step Uuid.uuidGenerator runState.randomSeed
    in
    { runState | randomSeed = newSeed, nextUuid = nextUuid }



-- }}}
-- {{{ Commands


type alias Commands msg =
    { navigate : Route -> Cmd msg
    }



-- }}}
-- {{{ Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- }}}
