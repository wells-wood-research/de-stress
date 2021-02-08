module TestDesign exposing (all)

import Codec
import Expect
import Set
import Shared.Buttons as Buttons
import Shared.Design as Design exposing (Design, DesignStub)
import Shared.Editable as Editable
import Shared.WebSockets as WebSockets
import Test exposing (..)


all : Test
all =
    describe "Design.elm"
        [ test "encoded/decoded designs are the same as input" <|
            \_ ->
                let
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
                    intendedStub : DesignStub
                    intendedStub =
                        testDesignStub
                in
                Expect.equal
                    intendedStub
                    (Design.createDesignStub testDesign)
        , test "encoded/decoded design stubs are the same as input" <|
            \_ ->
                let
                    encodedAndDecodedStub : Result Codec.Error DesignStub
                    encodedAndDecodedStub =
                        Codec.encoder
                            Design.designStubCodec
                            testDesignStub
                            |> Codec.decodeValue Design.designStubCodec
                in
                case encodedAndDecodedStub of
                    Ok stub ->
                        Expect.equal stub testDesignStub

                    Err _ ->
                        Expect.true
                            """Failed to decode the encoded design stub. This
                            should not happen, something went wrong with the codec."""
                            False
        ]


testDesign : Design
testDesign =
    { name = Editable.NotEditing "NAME"
    , fileName = "FILENAME"
    , pdbString = "PDBSTRING"
    , deleteStatus = Buttons.initDangerStatus
    , metricsJobStatus = WebSockets.initServerJobStatus
    , mMeetsActiveSpecification = Nothing
    , tags = Set.empty
    }


testDesignStub : DesignStub
testDesignStub =
    { name = "NAME"
    , fileName = "FILENAME"
    , deleteStatus = Buttons.initDangerStatus
    , metricsJobStatus = WebSockets.initServerJobStatus
    , mMeetsActiveSpecification = Nothing
    , tags = Set.empty
    }
