module TestResourceUuid exposing (all)

import Expect
import Fuzz exposing (int)
import Shared.ResourceUuid as ResourceUuid
import Test exposing (..)


all : Test
all =
    describe "Resource Uuid Generation"
        [ fuzz int "UUID Generation" <|
            \randomInt ->
                let
                    resourceUuid =
                        ResourceUuid.createInitialUuid randomInt

                    firstResourceUuid =
                        ResourceUuid.toString
                            resourceUuid

                    secondResourceUuid =
                        ResourceUuid.toString firstResourceUuid.nextResourceUuid
                in
                firstResourceUuid.uuidString
                    == secondResourceUuid.uuidString
                    |> Expect.false "Expected the old and new UUID to be different."
        ]
