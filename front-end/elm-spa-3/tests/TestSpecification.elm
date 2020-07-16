module TestSpecification exposing (..)

import Codec exposing (Codec)
import Expect
import Fuzz exposing (int)
import Specification exposing (Specification, SpecificationStub)
import Style
import Test exposing (..)


all : Test
all =
    describe "Specification.elm"
        [ test "encoded/decoded specifications are the same as input" <|
            \_ ->
                let
                    testSpec : Specification
                    testSpec =
                        { name = "NAME"
                        , description = "DESCRIPTION"
                        , requirements = Specification.All []
                        , deleteStatus = Style.Unclicked
                        }

                    encodedAndDecodedSpecification : Result Codec.Error Specification
                    encodedAndDecodedSpecification =
                        Codec.encoder
                            Specification.codec
                            testSpec
                            |> Codec.decodeValue Specification.codec
                in
                case encodedAndDecodedSpecification of
                    Ok specification ->
                        Expect.equal specification testSpec

                    Err _ ->
                        Expect.true
                            """Failed to decode the encoded specification. This should
                            not happen, something went wrong with the codec."""
                            False
        , test "stubs are created correctly" <|
            \_ ->
                let
                    testSpec : Specification
                    testSpec =
                        { name = "NAME"
                        , description = "DESCRIPTION"
                        , requirements = Specification.All []
                        , deleteStatus = Style.Unclicked
                        }

                    intendedStub : SpecificationStub
                    intendedStub =
                        { name = "NAME"
                        , description = "DESCRIPTION"
                        , deleteStatus = Style.Unclicked
                        }
                in
                Expect.equal
                    intendedStub
                    (Specification.createSpecificationStub testSpec)
        , test "encoded/decoded specification stubs are the same as input" <|
            \_ ->
                let
                    testStub : SpecificationStub
                    testStub =
                        { name = "NAME"
                        , description = "DESCRIPTION"
                        , deleteStatus = Style.Unclicked
                        }

                    encodedAndDecodedStub : Result Codec.Error SpecificationStub
                    encodedAndDecodedStub =
                        Codec.encoder
                            Specification.specificationStubCodec
                            testStub
                            |> Codec.decodeValue Specification.specificationStubCodec
                in
                case encodedAndDecodedStub of
                    Ok stub ->
                        Expect.equal stub testStub

                    Err _ ->
                        Expect.true
                            """Failed to decode the encoded specification stub. This
                            should not happen, something went wrong with the codec."""
                            False
        ]
