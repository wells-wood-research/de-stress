module Pages.Specifications.All exposing
    ( Model
    , Msg
    , page
    , requirementView
    )

import Application.Page as Page
import Codec exposing (Codec)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import FeatherIcons
import Global
import Specification as Spec exposing (Specification)
import Style exposing (h1)


type alias Model =
    List Specification


modelCodec : Codec Model
modelCodec =
    Codec.list Spec.specificationCodec


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


requirementView : Spec.Requirement Spec.RequirementData -> Element msg
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
        Spec.Data data ->
            case data of
                Spec.Constant constantType ->
                    let
                        typeString =
                            "Constant:"

                        requirementString =
                            case constantType of
                                Spec.Method methodType ->
                                    let
                                        constantTypeString =
                                            typeString ++ "Method:"
                                    in
                                    case methodType of
                                        Spec.SPPS ->
                                            constantTypeString ++ "SPPS"

                                        Spec.MolecularBiology ->
                                            constantTypeString ++ "MolBio"
                    in
                    el [] (text <| requirementString)

                Spec.Value valueType ->
                    let
                        typeString =
                            "Value:"

                        requirementString =
                            case valueType of
                                Spec.IsoelectricPoint order value ->
                                    typeString
                                        ++ "IsoelectricPoint:"
                                        ++ Spec.stringFromOrder
                                            order
                                        ++ ":"
                                        ++ String.fromFloat value

                                Spec.HydrophobicFitness order value ->
                                    typeString
                                        ++ "HydrophobicFitness:"
                                        ++ Spec.stringFromOrder
                                            order
                                        ++ ":"
                                        ++ String.fromFloat value

                                Spec.MeanPackingDensity order value ->
                                    typeString
                                        ++ "MeanPackingDensity:"
                                        ++ Spec.stringFromOrder
                                            order
                                        ++ ":"
                                        ++ String.fromFloat value

                                Spec.SequenceContains string ->
                                    typeString
                                        ++ "SequenceContains:"
                                        ++ string
                    in
                    el [] (text <| requirementString)

        Spec.Not subRequirement ->
            row [ spacing 10 ]
                [ Style.h3 <| el [ Font.bold ] (text <| "NOT")
                , requirementView subRequirement
                ]

        Spec.Or subRequirement1 subRequirement2 ->
            column
                (Style.defaultBorder ++ [ padding 15, spacing 10 ])
                [ requirementView subRequirement1
                , Style.h3 <| el [ Font.bold ] (text "---- OR ----")
                , requirementView subRequirement2
                ]

        Spec.And requirement1 requirement2 ->
            column
                (Style.defaultBorder ++ [ padding 15, spacing 10 ])
                [ requirementView requirement1
                , el [ Font.bold ]
                    (text "---- AND ----")
                , requirementView requirement2
                ]

        Spec.Any requirements ->
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

        Spec.All requirements ->
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
