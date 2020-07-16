module Shared.ResourceUuid exposing (ResourceUuid, createInitialUuid, toString)

import Random
import Uuid exposing (Uuid)


type ResourceUuid
    = ResourceUuid Uuid Random.Seed


createInitialUuid : Int -> ResourceUuid
createInitialUuid initialRandomNumber =
    let
        ( uuid, randomSeed ) =
            Random.initialSeed initialRandomNumber
                |> Random.step Uuid.uuidGenerator
    in
    ResourceUuid uuid randomSeed


stepResourceUuid : Random.Seed -> ResourceUuid
stepResourceUuid seed =
    let
        ( newUuid, newSeed ) =
            Random.step Uuid.uuidGenerator seed
    in
    ResourceUuid newUuid newSeed


toString : ResourceUuid -> { nextResourceUuid : ResourceUuid, uuidString : String }
toString (ResourceUuid uuid seed) =
    { uuidString = Uuid.toString uuid
    , nextResourceUuid = stepResourceUuid seed
    }
