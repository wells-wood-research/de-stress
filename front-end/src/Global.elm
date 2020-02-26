module Global exposing
    ( Flags
    , Model(..)
    , Msg(..)
    , createInitialUuid
    , init
    , storedDesignToStub
    , storedReferenceSetToStub
    , storedSpecificationToStub
    , subscriptions
    , update
    , updateUuid
    )

-- {{{ Imports

import BigStructure.Mutation as Mutation
import BigStructure.Object.CreateDesign as CreateDesign
import BigStructure.Object.Design as Design
import BigStructure.Object.DesignChain as DesignChain
import Codec exposing (Codec, Value)
import Design exposing (Design, DesignStub)
import Dict exposing (Dict)
import Generated.Routes exposing (Route, routes)
import Graphql.Http
import Graphql.Operation exposing (RootMutation)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Metrics exposing (DesignMetrics)
import Ports
import Random
import ReferenceSet exposing (ReferenceSet(..), ReferenceSetStub(..))
import RemoteData as RD
import Specification exposing (Specification, SpecificationStub)
import Style
import Uuid exposing (Uuid)



-- }}}
-- {{{ Flags


type alias Flags =
    Value


type alias DecodedFlags =
    { randomSeed : Int
    , mInitialState :
        Maybe
            { designs : Dict String StoredDesign
            , referenceSets : Dict String StoredReferenceSet
            , mSelectedReferenceSet : Maybe String
            , specifications : Dict String StoredSpecification
            , mSelectedSpecification : Maybe String
            }
    }


flagsCodec : Codec DecodedFlags
flagsCodec =
    Codec.object DecodedFlags
        |> Codec.field "randomSeed" .randomSeed Codec.int
        |> Codec.field "mInitialState"
            .mInitialState
            (storedStateCodec |> Codec.maybe)
        |> Codec.buildObject


type alias StoredState =
    { designs : Dict String StoredDesign
    , referenceSets : Dict String StoredReferenceSet
    , mSelectedReferenceSet : Maybe String
    , specifications : Dict String StoredSpecification
    , mSelectedSpecification : Maybe String
    }


storedStateCodec : Codec StoredState
storedStateCodec =
    Codec.object StoredState
        |> Codec.field "designs" .designs (Codec.dict storedDesignCodec)
        |> Codec.field "referenceSets"
            .referenceSets
            (Codec.dict storedReferenceSetCodec)
        |> Codec.field "mSelectedReferenceSet"
            .mSelectedReferenceSet
            (Codec.string
                |> Codec.maybe
            )
        |> Codec.field "specifications"
            .specifications
            (Codec.dict storedSpecificationCodec)
        |> Codec.field "mSelectedSpecification"
            .mSelectedSpecification
            (Codec.string
                |> Codec.maybe
            )
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


type StoredReferenceSet
    = LocalReferenceSet ReferenceSetStub


storedReferenceSetCodec : Codec StoredReferenceSet
storedReferenceSetCodec =
    Codec.custom
        (\flocal value ->
            case value of
                LocalReferenceSet stub ->
                    flocal stub
        )
        |> Codec.variant1 "LocalReferenceSet"
            LocalReferenceSet
            ReferenceSet.referenceSetStubCodec
        |> Codec.buildCustom


storedReferenceSetToStub : StoredReferenceSet -> ReferenceSetStub
storedReferenceSetToStub storedReferenceSet =
    case storedReferenceSet of
        LocalReferenceSet stub ->
            stub


mapStoredReferenceSet :
    (ReferenceSetStub -> ReferenceSetStub)
    -> StoredReferenceSet
    -> StoredReferenceSet
mapStoredReferenceSet stubFn storedReferenceSet =
    case storedReferenceSet of
        LocalReferenceSet stub ->
            stubFn stub |> LocalReferenceSet


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
    , referenceSets : Dict String StoredReferenceSet
    , mSelectedReferenceSet : Maybe String
    , specifications : Dict String StoredSpecification
    , mSelectedSpecification : Maybe String
    }


encodeStoredState : StoredState -> Value
encodeStoredState storedState =
    Codec.encoder
        storedStateCodec
        storedState


type LaunchError
    = FailedToDecodeFlags Codec.Error


type alias DesignAndKey =
    { storeKey : String
    , design : Design
    }


encodeDesignAndKey : DesignAndKey -> Value
encodeDesignAndKey designAndKey =
    Codec.encoder
        designAndKeyCodec
        designAndKey


designAndKeyCodec : Codec DesignAndKey
designAndKeyCodec =
    Codec.object DesignAndKey
        |> Codec.field "storeKey" .storeKey Codec.string
        |> Codec.field "design" .design Design.codec
        |> Codec.buildObject


type alias DesignMetricsRDAndKey =
    { storeKey : String
    , designMetricsRD : Metrics.DesMetricsRemoteData
    }


encodeDesignMetricsRDAndKey : DesignMetricsRDAndKey -> Value
encodeDesignMetricsRDAndKey designMetricsRDAndKey =
    Codec.encoder
        designMetricsRDAndKeyCodec
        designMetricsRDAndKey


designMetricsRDAndKeyCodec : Codec DesignMetricsRDAndKey
designMetricsRDAndKeyCodec =
    Codec.object DesignMetricsRDAndKey
        |> Codec.field "storeKey" .storeKey Codec.string
        |> Codec.field "designMetricsRD" .designMetricsRD Design.metricsRDCodec
        |> Codec.buildObject


type alias ReferenceSetAndKey =
    { storeKey : String
    , referenceSet : ReferenceSet
    }


encodeReferenceSetAndKey : ReferenceSetAndKey -> Value
encodeReferenceSetAndKey referenceSetAndKey =
    Codec.encoder
        referenceSetAndKeyCodec
        referenceSetAndKey


referenceSetAndKeyCodec : Codec ReferenceSetAndKey
referenceSetAndKeyCodec =
    Codec.object ReferenceSetAndKey
        |> Codec.field "storeKey" .storeKey Codec.string
        |> Codec.field "referenceSet" .referenceSet ReferenceSet.codec
        |> Codec.buildObject


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
                        , referenceSets = initialState.referenceSets
                        , mSelectedReferenceSet =
                            initialState.mSelectedReferenceSet
                        , specifications = initialState.specifications
                        , mSelectedSpecification =
                            initialState.mSelectedSpecification
                        }

                Nothing ->
                    Running
                        { randomSeed = randomSeed
                        , nextUuid = nextUuid
                        , designs = Dict.empty
                        , referenceSets = Dict.empty
                        , mSelectedReferenceSet = Nothing
                        , specifications = Dict.empty
                        , mSelectedSpecification = Nothing
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
    | GotDesignMetrics String Metrics.DesMetricsRemoteData
    | DeleteDesign String Style.DangerStatus
    | GetDesign String
    | DeleteFocussedDesign String Style.DangerStatus
    | DeleteAllDesigns Style.DangerStatus
    | AddReferenceSet ReferenceSet
    | AddNamedReferenceSet String ReferenceSet
    | DeleteReferenceSet String Style.DangerStatus
    | DeleteFocussedReferenceSet String Style.DangerStatus
    | SetMSelectedReferenceSet (Maybe String)
    | AddSpecification Specification
    | DeleteSpecification String Style.DangerStatus
    | GetSpecification String
    | DeleteFocussedSpecification String Style.DangerStatus
    | SetMSelectedSpecification (Maybe String)
    | RequestedNewUuid


update : Commands msg -> Msg -> Model -> ( Model, Cmd Msg, Cmd msg )
update commands msg model =
    case model of
        Running runState ->
            updateRunState commands msg runState
                |> addStoreCmd
                |> asModel Running

        FailedToLaunch _ ->
            case msg of
                _ ->
                    ( model, Cmd.none, Cmd.none )


asModel : (a -> Model) -> ( a, Cmd Msg, Cmd msg ) -> ( Model, Cmd Msg, Cmd msg )
asModel constructor ( state, gCmd, pCmd ) =
    ( constructor state, gCmd, pCmd )


updateRunState : Commands msg -> Msg -> RunState -> ( RunState, Cmd Msg, Cmd msg )
updateRunState commands msg runState =
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
            , requestDesignMetrics { uuid = uuidString, pdbString = design.pdbString }
            , encodeDesignAndKey
                { storeKey = uuidString
                , design = design
                }
                |> Ports.storeDesign
            )

        GotDesignMetrics uuid metricsRemoteData ->
            ( { runState
                | designs =
                    Dict.update
                        uuid
                        (Maybe.map <|
                            mapStoredDesign <|
                                \designStub ->
                                    { designStub
                                        | metricsRemoteData =
                                            metricsRemoteData
                                    }
                        )
                        runState.designs
              }
            , encodeDesignMetricsRDAndKey
                { storeKey = uuid
                , designMetricsRD =
                    metricsRemoteData
                }
                |> Ports.updateDesignMetricsRD
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
                    , Codec.encoder Codec.string uuidString
                        |> Ports.deleteDesign
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

        DeleteFocussedDesign uuidString dangerStatus ->
            case dangerStatus of
                Style.Confirmed ->
                    ( { runState
                        | designs =
                            Dict.remove uuidString runState.designs
                      }
                    , Cmd.none
                    , Cmd.batch
                        [ Codec.encoder Codec.string uuidString
                            |> Ports.deleteDesign
                        , commands.navigate routes.designs
                        ]
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

        DeleteAllDesigns dangerStatus ->
            case dangerStatus of
                Style.Confirmed ->
                    ( { runState
                        | designs =
                            Dict.empty
                      }
                    , Cmd.none
                    , Dict.keys runState.designs
                        |> List.map
                            (\uuidString ->
                                Codec.encoder Codec.string uuidString
                                    |> Ports.deleteDesign
                            )
                        |> Cmd.batch
                    )

                _ ->
                    ( runState
                    , Cmd.none
                    , Cmd.none
                    )

        SetMSelectedReferenceSet mId ->
            ( { runState | mSelectedReferenceSet = mId }
            , Cmd.none
            , Cmd.none
            )

        GetDesign uuidString ->
            ( runState
            , Cmd.none
            , Codec.encoder Codec.string uuidString
                |> Ports.getDesign
            )

        AddReferenceSet referenceSet ->
            let
                uuidString =
                    Uuid.toString runState.nextUuid
            in
            ( { runState
                | referenceSets =
                    Dict.insert
                        uuidString
                        (referenceSet
                            |> ReferenceSet.createReferenceSetStub
                            |> LocalReferenceSet
                        )
                        runState.referenceSets
              }
                |> updateUuid
            , Cmd.none
            , encodeReferenceSetAndKey
                { storeKey = uuidString
                , referenceSet = referenceSet
                }
                |> Ports.storeReferenceSet
            )

        AddNamedReferenceSet name referenceSet ->
            ( { runState
                | referenceSets =
                    Dict.insert
                        name
                        (referenceSet
                            |> ReferenceSet.createReferenceSetStub
                            |> LocalReferenceSet
                        )
                        runState.referenceSets
              }
                |> updateUuid
            , Cmd.none
            , Cmd.batch
                [ encodeReferenceSetAndKey
                    { storeKey = name
                    , referenceSet = referenceSet
                    }
                    |> Ports.storeReferenceSet
                , commands.navigate routes.referenceSets
                ]
            )

        DeleteReferenceSet uuidString dangerStatus ->
            case dangerStatus of
                Style.Confirmed ->
                    ( { runState
                        | referenceSets =
                            Dict.remove uuidString runState.referenceSets
                      }
                    , Cmd.none
                    , Codec.encoder Codec.string uuidString
                        |> Ports.deleteReferenceSet
                    )

                _ ->
                    ( { runState
                        | referenceSets =
                            Dict.update
                                uuidString
                                ((\r ->
                                    case r of
                                        HighResBiolUnitStub _ ->
                                            HighResBiolUnitStub dangerStatus

                                        PdbCodeListStub params ->
                                            PdbCodeListStub
                                                { params
                                                    | deleteStatus =
                                                        dangerStatus
                                                }
                                 )
                                    |> mapStoredReferenceSet
                                    |> Maybe.map
                                )
                                runState.referenceSets
                      }
                    , Cmd.none
                    , Cmd.none
                    )

        DeleteFocussedReferenceSet uuidString dangerStatus ->
            case dangerStatus of
                Style.Confirmed ->
                    ( { runState
                        | referenceSets =
                            Dict.remove uuidString runState.referenceSets
                      }
                    , Cmd.none
                    , Cmd.batch
                        [ Codec.encoder Codec.string uuidString
                            |> Ports.deleteReferenceSet
                        , commands.navigate routes.referenceSets
                        ]
                    )

                _ ->
                    ( { runState
                        | referenceSets =
                            Dict.update
                                uuidString
                                ((\r ->
                                    case r of
                                        HighResBiolUnitStub _ ->
                                            HighResBiolUnitStub dangerStatus

                                        PdbCodeListStub params ->
                                            PdbCodeListStub
                                                { params
                                                    | deleteStatus =
                                                        dangerStatus
                                                }
                                 )
                                    |> mapStoredReferenceSet
                                    |> Maybe.map
                                )
                                runState.referenceSets
                      }
                    , Cmd.none
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
            , Cmd.batch
                [ encodeSpecificationAndKey
                    { storeKey = uuidString
                    , specification = spec
                    }
                    |> Ports.storeSpecification
                , commands.navigate routes.specifications
                ]
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

        DeleteFocussedSpecification uuidString dangerStatus ->
            case dangerStatus of
                Style.Confirmed ->
                    ( { runState
                        | specifications =
                            Dict.remove uuidString runState.specifications
                      }
                    , Cmd.none
                    , Cmd.batch
                        [ Codec.encoder Codec.string uuidString
                            |> Ports.deleteSpecification
                        , commands.navigate routes.specifications
                        ]
                    )

                _ ->
                    ( { runState
                        | specifications =
                            Dict.update
                                uuidString
                                ((\d ->
                                    { d
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

        SetMSelectedSpecification mId ->
            ( { runState | mSelectedSpecification = mId }
            , Cmd.none
            , Cmd.none
            )

        RequestedNewUuid ->
            ( runState, Cmd.none, Cmd.none )


addStoreCmd : ( RunState, Cmd Msg, Cmd msg ) -> ( RunState, Cmd Msg, Cmd msg )
addStoreCmd ( state, gCmd, pCmd ) =
    ( state
    , Cmd.batch
        [ gCmd
        , encodeStoredState
            { designs = state.designs
            , referenceSets = state.referenceSets
            , mSelectedReferenceSet = state.mSelectedReferenceSet
            , specifications = state.specifications
            , mSelectedSpecification = state.mSelectedSpecification
            }
            |> Ports.storeRunState
        ]
    , pCmd
    )


updateUuid : RunState -> RunState
updateUuid runState =
    let
        ( nextUuid, newSeed ) =
            Random.step Uuid.uuidGenerator runState.randomSeed
    in
    { runState | randomSeed = newSeed, nextUuid = nextUuid }


createDesignMutation :
    { uuid : String, pdbString : String }
    -> SelectionSet DesignMetrics RootMutation
createDesignMutation requiredArgs =
    Mutation.createDesign
        requiredArgs
        (CreateDesign.design <|
            SelectionSet.map8 DesignMetrics
                (SelectionSet.map2
                    (\mLabels mSeqs ->
                        let
                            labels =
                                List.filterMap identity mLabels

                            seqs =
                                List.filterMap identity mSeqs
                        in
                        List.map2 Tuple.pair labels seqs
                            |> Dict.fromList
                    )
                    (Design.chains DesignChain.chainLabel
                        |> SelectionSet.map (Maybe.withDefault [])
                    )
                    (Design.chains DesignChain.sequence
                        |> SelectionSet.map (Maybe.withDefault [])
                    )
                )
                (SelectionSet.map Metrics.compositionStringToDict Design.composition)
                (SelectionSet.map Metrics.torsionAngleStringToDict Design.torsionAngles)
                Design.hydrophobicFitness
                Design.isoelectricPoint
                Design.mass
                Design.numOfResidues
                Design.meanPackingDensity
        )


requestDesignMetrics : { uuid : String, pdbString : String } -> Cmd Msg
requestDesignMetrics requiredArgs =
    createDesignMutation requiredArgs
        |> Graphql.Http.mutationRequest "http://127.0.0.1:5000/graphql"
        |> Graphql.Http.send (RD.fromResult >> GotDesignMetrics requiredArgs.uuid)



-- }}}
-- {{{ Commands


type alias Commands msg =
    { navigate : Route -> Cmd msg
    }



-- }}}
-- {{{ Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        []



-- }}}
