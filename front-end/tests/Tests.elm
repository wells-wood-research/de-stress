module Tests exposing (..)

import Dict
import Expect
import Fuzz exposing (int)
import Global
import Random
import Test exposing (..)
import Uuid


all : Test
all =
    describe "DE-STRESS Test suite"
        [ fuzz int "UUID Generation" <|
            \randomInt ->
                let
                    ( firstUuid, firstSeed ) =
                        Global.createInitialUuid randomInt

                    newSeedAndUuid =
                        Global.updateUuid
                            { randomSeed = firstSeed
                            , nextUuid =
                                firstUuid
                            , designs = Dict.empty
                            , specifications = Dict.empty
                            }
                in
                newSeedAndUuid.nextUuid
                    == firstUuid
                    |> Expect.false "Expected the old and new UUID to be different."
        ]
