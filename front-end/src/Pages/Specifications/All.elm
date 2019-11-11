module Pages.Specifications.All exposing
    ( ConstantType(..)
    , MethodType(..)
    , Model
    , Msg
    , Requirement(..)
    , RequirementData(..)
    , ValueType(..)
    , page
    , requirementView
    , stringFromOrder
    )

import Application.Page as Page
import Codec exposing (Codec)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import FeatherIcons
import Global
import Style exposing (h1)


type alias Model =
    List Specification


modelCodec : Codec Model
modelCodec =
    Codec.list specificationCodec


type alias Specification =
    { name : String
    , description : String
    , requirements : Requirement RequirementData
    , deleteStatus : Style.DangerStatus
    }


specificationCodec : Codec Specification
specificationCodec =
    Codec.object Specification
        |> Codec.field "name" .name Codec.string
        |> Codec.field "description" .description Codec.string
        |> Codec.field "requirements" .requirements (requirementCodec requirementDataTypeCodec)
        |> Codec.field "deleteStatus" .deleteStatus (Codec.constant Style.Unclicked)
        |> Codec.buildObject


type Requirement a
    = Data a
    | Not (Requirement a)
    | Or (Requirement a) (Requirement a)
    | And (Requirement a) (Requirement a)
    | Any (List (Requirement a))
    | All (List (Requirement a))


requirementCodec : Codec a -> Codec (Requirement a)
requirementCodec meta =
    Codec.recursive
        (\rmeta ->
            Codec.custom
                (\fdata fnot for fand fany fall value ->
                    case value of
                        Data data ->
                            fdata data

                        Not requirement ->
                            fnot requirement

                        Or requirement1 requirement2 ->
                            for requirement1 requirement2

                        And requirement1 requirement2 ->
                            fand requirement1 requirement2

                        Any requirements ->
                            fany requirements

                        All requirements ->
                            fall requirements
                )
                |> Codec.variant1 "Data" Data meta
                |> Codec.variant1 "Not" Not rmeta
                |> Codec.variant2 "Or" Or rmeta rmeta
                |> Codec.variant2 "And" And rmeta rmeta
                |> Codec.variant1 "Any" Any (Codec.list rmeta)
                |> Codec.variant1 "All" All (Codec.list rmeta)
                |> Codec.buildCustom
        )


type RequirementData
    = Constant ConstantType
    | Value ValueType


requirementDataTypeCodec : Codec RequirementData
requirementDataTypeCodec =
    Codec.custom
        (\fconstant fvalue value ->
            case value of
                Constant constantType ->
                    fconstant constantType

                Value valueType ->
                    fvalue valueType
        )
        |> Codec.variant1 "Constant" Constant constantTypeCodec
        |> Codec.variant1 "Value" Value valueTypeCodec
        |> Codec.buildCustom


type ConstantType
    = Method MethodType


constantTypeCodec : Codec ConstantType
constantTypeCodec =
    Codec.custom
        (\fconstantType value ->
            case value of
                Method methodType ->
                    fconstantType methodType
        )
        |> Codec.variant1 "Method" Method methodTypeCodec
        |> Codec.buildCustom


type MethodType
    = SPPS
    | MolecularBiology


methodTypeCodec : Codec MethodType
methodTypeCodec =
    Codec.custom
        (\fspps fmolbio value ->
            case value of
                SPPS ->
                    fspps

                MolecularBiology ->
                    fmolbio
        )
        |> Codec.variant0 "SPPS" SPPS
        |> Codec.variant0 "MolecularBiology" MolecularBiology
        |> Codec.buildCustom


type ValueType
    = IsoelectricPoint Order Float
    | HydrophobicFitness Order Float
    | MeanPackingDensity Order Float
    | SequenceContains String


valueTypeCodec : Codec ValueType
valueTypeCodec =
    Codec.custom
        (\fisoelectric fhfitness fmpdensity fseqcontains value ->
            case value of
                IsoelectricPoint order float ->
                    fisoelectric order float

                HydrophobicFitness order float ->
                    fhfitness order float

                MeanPackingDensity order float ->
                    fmpdensity order float

                SequenceContains sequence ->
                    fseqcontains sequence
        )
        |> Codec.variant2 "IsoelectricPoint" IsoelectricPoint orderCodec Codec.float
        |> Codec.variant2 "HydrophobicFitness" HydrophobicFitness orderCodec Codec.float
        |> Codec.variant2 "MeanPackingDensity" MeanPackingDensity orderCodec Codec.float
        |> Codec.variant1 "SequenceContains" SequenceContains Codec.string
        |> Codec.buildCustom


orderCodec : Codec Order
orderCodec =
    Codec.custom
        (\flt feq fgt value ->
            case value of
                LT ->
                    flt

                EQ ->
                    feq

                GT ->
                    fgt
        )
        |> Codec.variant0 "LT" LT
        |> Codec.variant0 "EQ" EQ
        |> Codec.variant0 "GT" GT
        |> Codec.buildCustom


stringFromOrder : Order -> String
stringFromOrder order =
    case order of
        LT ->
            "LessThan"

        EQ ->
            "EqualTo"

        GT ->
            "GreaterThan"


type Msg
    = NoOp


page =
    Page.component
        { title = title
        , init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


title : Global.Model -> Model -> String
title _ _ =
    "Specifications"


init : Global.Model -> () -> ( Model, Cmd Msg, Cmd Global.Msg )
init _ _ =
    ( []
    , Cmd.none
    , Cmd.none
    )


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update _ msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            , Cmd.none
            )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none


view : Global.Model -> Model -> Element Msg
view _ _ =
    h1 <| text "Specifications"


requirementView : Requirement RequirementData -> Element msg
requirementView requirement =
    let
        arrowRow r =
            row [ padding 5, spacing 15 ]
                [ el []
                    (FeatherIcons.chevronRight
                        |> FeatherIcons.toHtml []
                        |> html
                    )
                , requirementView r
                ]
    in
    case requirement of
        Data data ->
            case data of
                Constant constantType ->
                    let
                        typeString =
                            "Constant:"

                        requirementString =
                            case constantType of
                                Method methodType ->
                                    let
                                        constantTypeString =
                                            typeString ++ "Method:"
                                    in
                                    case methodType of
                                        SPPS ->
                                            constantTypeString ++ "SPPS"

                                        MolecularBiology ->
                                            constantTypeString ++ "MolBio"
                    in
                    el [] (text <| requirementString)

                Value valueType ->
                    let
                        typeString =
                            "Value:"

                        requirementString =
                            case valueType of
                                IsoelectricPoint order value ->
                                    typeString
                                        ++ "IsoelectricPoint:"
                                        ++ stringFromOrder
                                            order
                                        ++ ":"
                                        ++ String.fromFloat value

                                HydrophobicFitness order value ->
                                    typeString
                                        ++ "HydrophobicFitness:"
                                        ++ stringFromOrder
                                            order
                                        ++ ":"
                                        ++ String.fromFloat value

                                MeanPackingDensity order value ->
                                    typeString
                                        ++ "MeanPackingDensity:"
                                        ++ stringFromOrder
                                            order
                                        ++ ":"
                                        ++ String.fromFloat value

                                SequenceContains string ->
                                    typeString
                                        ++ "SequenceContains:"
                                        ++ string
                    in
                    el [] (text <| requirementString)

        Not subRequirement ->
            row [ spacing 10 ]
                [ Style.h3 <| el [ Font.bold ] (text <| "NOT")
                , requirementView subRequirement
                ]

        Or subRequirement1 subRequirement2 ->
            column
                (Style.defaultBorder ++ [ padding 15, spacing 10 ])
                [ requirementView subRequirement1
                , Style.h3 <| el [ Font.bold ] (text "---- OR ----")
                , requirementView subRequirement2
                ]

        And requirement1 requirement2 ->
            column
                (Style.defaultBorder ++ [ padding 15, spacing 10 ])
                [ requirementView requirement1
                , el [ Font.bold ]
                    (text "---- AND ----")
                , requirementView requirement2
                ]

        Any requirements ->
            column
                (Style.defaultBorder
                    ++ [ padding 15
                       , spacing 10
                       ]
                )
                [ el [ Font.bold ] (text "ANY")
                , column [ padding 15, spacing 10 ] <|
                    List.map arrowRow requirements
                ]

        All requirements ->
            column
                (Style.defaultBorder
                    ++ [ padding 15
                       , spacing 10
                       , Background.color
                            Style.colorPalette.white
                       ]
                )
                [ el [ Font.bold ] (text "ALL")
                , column [ padding 10, spacing 5 ] <|
                    List.map arrowRow requirements
                ]
