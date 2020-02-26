module TestDesign exposing (all)

import Codec
import Design exposing (Design, DesignStub)
import Expect
import RemoteData as RD
import Style
import Test exposing (..)


all : Test
all =
    describe "Design.elm"
        [ test "encoded/decoded designs are the same as input" <|
            \_ ->
                let
                    testDesign : Design
                    testDesign =
                        { name = Design.NotEditing "NAME"
                        , fileName = "FILENAME"
                        , pdbString = "PDBSTRING"
                        , deleteStatus = Style.Unclicked
                        , metricsRemoteData = RD.NotAsked
                        , mMeetsActiveSpecification = Nothing
                        }

                    encodedAndDecodedDesign : Result Codec.Error Design
                    encodedAndDecodedDesign =
                        Codec.encoder
                            Design.codec
                            testDesign
                            |> Codec.decodeValue Design.codec
                in
                case encodedAndDecodedDesign of
                    Ok design ->
                        Expect.equal design testDesign

                    Err _ ->
                        Expect.true
                            """Failed to decode the encoded design. This should
                            not happen, something went wrong with the codec."""
                            False
        , test "stubs are created correctly" <|
            \_ ->
                let
                    testDesign : Design
                    testDesign =
                        { name = Design.NotEditing "NAME"
                        , fileName = "FILENAME"
                        , pdbString = "PDBSTRING"
                        , deleteStatus = Style.Unclicked
                        , metricsRemoteData = RD.NotAsked
                        , mMeetsActiveSpecification = Nothing
                        }

                    intendedStub : DesignStub
                    intendedStub =
                        { name = "NAME"
                        , fileName = "FILENAME"
                        , deleteStatus = Style.Unclicked
                        , metricsRemoteData = RD.NotAsked
                        , mMeetsActiveSpecification = Nothing
                        }
                in
                Expect.equal
                    intendedStub
                    (Design.createDesignStub testDesign)
        , test "encoded/decoded design stubs are the same as input" <|
            \_ ->
                let
                    testStub : DesignStub
                    testStub =
                        { name = "NAME"
                        , fileName = "FILENAME"
                        , deleteStatus = Style.Unclicked
                        , metricsRemoteData = RD.NotAsked
                        , mMeetsActiveSpecification = Nothing
                        }

                    encodedAndDecodedStub : Result Codec.Error DesignStub
                    encodedAndDecodedStub =
                        Codec.encoder
                            Design.designStubCodec
                            testStub
                            |> Codec.decodeValue Design.designStubCodec
                in
                case encodedAndDecodedStub of
                    Ok stub ->
                        Expect.equal stub testStub

                    Err _ ->
                        Expect.true
                            """Failed to decode the encoded design stub. This
                            should not happen, something went wrong with the codec."""
                            False
        ]
