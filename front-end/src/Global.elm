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
    , Ports.log "Hello!"
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
    = Msg


update : Commands msg -> Msg -> Model -> ( Model, Cmd Msg, Cmd msg )
update _ _ model =
    ( model
    , Cmd.none
    , Cmd.none
    )



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
