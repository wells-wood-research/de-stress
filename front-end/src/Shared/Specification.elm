port module Shared.Specification exposing
    ( Specification
    , SpecificationStub
    , StoredSpecification
    , applySpecification
    , codec
    , createSpecificationStub
    , deleteSpecification
    , getStoredSpecification
    , mapStoredSpecification
    , specificationStubCodec
    , storeSpecification
    , storeSpecificationStubLocally
    , storedSpecificationCodec
    , storedSpecificationToStub
    )

import Codec exposing (Codec, Value)
import Element exposing (..)
import Shared.Buttons as Buttons exposing (DangerStatus)
import Shared.Metrics as Metrics exposing (DesignMetrics)
import Shared.Requirement as Requirement exposing (Requirement, RequirementData)



-- {{{ PORTS


port storeSpecification : { uuidString : String, specification : Value } -> Cmd msg


port getStoredSpecification : { uuidString : String } -> Cmd msg


port deleteSpecification : { uuidString : String } -> Cmd msg



-- }}}
-- {{{ SPECIFICATION


type alias Specification =
    { name : String
    , description : String
    , requirements : Requirement RequirementData
    , deleteStatus : DangerStatus
    }


applySpecification : Maybe Metrics.AggregateData -> DesignMetrics -> Specification -> Bool
applySpecification mAggregateData designMetrics specification =
    Requirement.resolveRequirement mAggregateData designMetrics specification.requirements


codec : Codec Specification
codec =
    Codec.object Specification
        |> Codec.field "name" .name Codec.string
        |> Codec.field "description" .description Codec.string
        |> Codec.field "requirements"
            .requirements
            Requirement.requirementWithDataCodec
        |> Codec.field "deleteStatus"
            .deleteStatus
            (Codec.constant
                Buttons.initDangerStatus
            )
        |> Codec.buildObject



-- }}}
-- {{{ SPECIFICATION STUB


type alias SpecificationStub =
    { name : String
    , description : String
    , deleteStatus : DangerStatus
    }


createSpecificationStub : Specification -> SpecificationStub
createSpecificationStub specification =
    { name = specification.name
    , description = specification.description
    , deleteStatus = specification.deleteStatus
    }


specificationStubCodec : Codec SpecificationStub
specificationStubCodec =
    Codec.object SpecificationStub
        |> Codec.field "name" .name Codec.string
        |> Codec.field "description" .description Codec.string
        |> Codec.field "deleteStatus"
            .deleteStatus
            (Codec.constant
                Buttons.initDangerStatus
            )
        |> Codec.buildObject



-- }}}
-- {{{ STORED SPECIFICATION


type StoredSpecification
    = LocalSpecification SpecificationStub


storeSpecificationStubLocally : SpecificationStub -> StoredSpecification
storeSpecificationStubLocally stub =
    LocalSpecification stub


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


storedSpecificationCodec : Codec StoredSpecification
storedSpecificationCodec =
    Codec.custom
        (\flocal value ->
            case value of
                LocalSpecification stub ->
                    flocal stub
        )
        |> Codec.variant1 "LocalSpecification" LocalSpecification specificationStubCodec
        |> Codec.buildCustom



-- }}}
