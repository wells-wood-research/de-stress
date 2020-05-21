module Global exposing
    ( Flags
    , Model(..)
    , Msg(..)
    , WebSocketConnectionStatus(..)
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
import Graphql.Operation exposing (RootMutation)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Metrics exposing (DesignMetrics)
import Ports
import Random
import ReferenceSet exposing (ReferenceSet(..), ReferenceSetStub(..))
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
    , webSocketConnectionStatus : WebSocketConnectionStatus
    , designs : Dict String StoredDesign
    , referenceSets : Dict String StoredReferenceSet
    , mSelectedReferenceSet : Maybe String
    , specifications : Dict String StoredSpecification
    , mSelectedSpecification : Maybe String
    }


type WebSocketConnectionStatus
    = Unknown
    | Disconnected
    | Connected


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


type alias MetricsJobStatusAndKey =
    { storeKey : String
    , metricsJobStatus : Ports.MetricsServerJobStatus
    }


encodeDesignMetricsJobAndKey : MetricsJobStatusAndKey -> Value
encodeDesignMetricsJobAndKey metricsJobStatusAndKey =
    Codec.encoder
        metricsJobStatusAndKeyCodec
        metricsJobStatusAndKey


metricsJobStatusAndKeyCodec : Codec MetricsJobStatusAndKey
metricsJobStatusAndKeyCodec =
    Codec.object MetricsJobStatusAndKey
        |> Codec.field "storeKey" .storeKey Codec.string
        |> Codec.field "metricsJobStatus" .metricsJobStatus Ports.metricsServerJobStatusCodec
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
                        , webSocketConnectionStatus = Unknown
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
                        , webSocketConnectionStatus = Unknown
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
    | UpdateDesignMetricsJob Ports.MetricsServerJob
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
    | SetWebSocketConnectionStatus Value
    | WebSocketIncoming Value


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

                metricsServerJob =
                    Ports.newMetricsServerJob { uuid = uuidString, pdbString = design.pdbString }

                submittedDesign =
                    { design | metricsJobStatus = metricsServerJob.status }
            in
            ( { runState
                | designs =
                    Dict.insert
                        uuidString
                        (submittedDesign
                            |> Design.createDesignStub
                            |> LocalDesign
                        )
                        runState.designs
              }
                |> updateUuid
            , Cmd.batch
                [ metricsServerJob
                    |> Ports.RequestMetrics
                    |> Ports.websocketOutgoingToCmd
                ]
            , encodeDesignAndKey
                { storeKey = uuidString
                , design = submittedDesign
                }
                |> Ports.storeDesign
            )

        UpdateDesignMetricsJob metricsServerJob ->
            ( { runState
                | designs =
                    Dict.update
                        metricsServerJob.uuid
                        (Maybe.map <|
                            mapStoredDesign <|
                                \designStub ->
                                    { designStub
                                        | metricsJobStatus =
                                            metricsServerJob.status
                                    }
                        )
                        runState.designs
              }
            , encodeDesignMetricsJobAndKey
                { storeKey = metricsServerJob.uuid
                , metricsJobStatus = metricsServerJob.status
                }
                |> Ports.updateMetricsJobStatus
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

        SetWebSocketConnectionStatus value ->
            let
                updatedConnectionStatus =
                    case Codec.decodeValue Codec.bool value of
                        Ok bool ->
                            if bool then
                                Connected

                            else
                                Disconnected

                        Err errString ->
                            let
                                _ =
                                    Debug.log "Err" errString
                            in
                            Unknown
            in
            ( { runState | webSocketConnectionStatus = updatedConnectionStatus }
            , Cmd.none
            , Cmd.none
            )

        WebSocketIncoming value ->
            let
                webSocketIncomingAction =
                    case Codec.decodeValue Ports.websocketIncomingCodec value of
                        Ok val ->
                            val

                        Err errString ->
                            let
                                _ =
                                    Debug.log "Err" errString
                            in
                            Ports.CommunicationError
            in
            case webSocketIncomingAction of
                Ports.ReceivedMetricsJob serverJob ->
                    updateRunState
                        commands
                        (UpdateDesignMetricsJob serverJob)
                        runState

                Ports.CommunicationError ->
                    Debug.todo "Deal with this!"


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
        [ Ports.setWebSocketConnectionStatus SetWebSocketConnectionStatus
        , Ports.webSocketIncoming WebSocketIncoming
        ]



-- }}}
