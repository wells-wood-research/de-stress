module Pages.Specifications exposing (Model, Msg, page, requirementView)

import Codec exposing (Codec, Value)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import FeatherIcons
import Generated.Params as Params
import Global
import Ports
import Spa
import Spa.Page exposing (send)
import Specification as Spec exposing (Specification, SpecificationStub)
import Style exposing (h1)
import Utils.Spa exposing (Page)


page : Page Params.Specifications Model Msg model msg appMsg
page =
    Spa.Page.component
        { title = always "Specifications"
        , init = always init
        , update = always update
        , subscriptions = always subscriptions
        , view = view
        }



-- {{{ Init


type alias Model =
    { focussedSpecification : FocussedSpecifiation }


type FocussedSpecifiation
    = NoFocus
    | Loading
    | Spec String Specification


mapFocussedSpecification :
    ({ uuidString : String, specification : Specification }
     -> { uuidString : String, specification : Specification }
    )
    -> FocussedSpecifiation
    -> FocussedSpecifiation
mapFocussedSpecification specFn focus =
    case focus of
        NoFocus ->
            focus

        Loading ->
            focus

        Spec uuidString specification ->
            { uuidString = uuidString, specification = specification }
                |> specFn
                |> (\idSpec -> Spec idSpec.uuidString idSpec.specification)


init : Params.Specifications -> ( Model, Cmd Msg, Cmd Global.Msg )
init _ =
    ( { focussedSpecification = NoFocus }
    , Cmd.none
    , Cmd.none
    )



-- }}}
-- {{{ Update


type Msg
    = ClickedFocusSpecification String
    | SetFocus Value
    | DeleteSpecification String Style.DangerStatus
    | DeleteFocussedSpecification String Style.DangerStatus
    | ClearFocus


update : Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update msg model =
    case msg of
        ClickedFocusSpecification uuidString ->
            ( { model | focussedSpecification = Loading }
            , Cmd.none
            , Global.GetSpecification uuidString
                |> send
            )

        SetFocus value ->
            let
                focusCodec =
                    Codec.object
                        (\uuidString spec ->
                            { uuidString = uuidString
                            , specification = spec
                            }
                        )
                        |> Codec.field "uuidString" .uuidString Codec.string
                        |> Codec.field "specification"
                            .specification
                            Spec.codec
                        |> Codec.buildObject
            in
            case Codec.decodeValue focusCodec value of
                Ok { uuidString, specification } ->
                    ( { model | focussedSpecification = Spec uuidString specification }
                    , Cmd.none
                    , Cmd.none
                    )

                Err errorString ->
                    Debug.todo "Catch this error"

        ClearFocus ->
            ( { model | focussedSpecification = NoFocus }
            , Cmd.none
            , Cmd.none
            )

        DeleteSpecification uuidString dangerStatus ->
            ( model
            , Cmd.none
            , Global.DeleteSpecification uuidString dangerStatus
                |> send
            )

        DeleteFocussedSpecification globalUuidString dangerStatus ->
            ( case dangerStatus of
                Style.Confirmed ->
                    { model
                        | focussedSpecification =
                            NoFocus
                    }

                _ ->
                    { model
                        | focussedSpecification =
                            mapFocussedSpecification
                                (\{ uuidString, specification } ->
                                    { uuidString = uuidString
                                    , specification =
                                        { specification
                                            | deleteStatus =
                                                dangerStatus
                                        }
                                    }
                                )
                                model.focussedSpecification
                    }
            , Cmd.none
            , Global.DeleteSpecification globalUuidString dangerStatus
                |> send
            )



-- }}}
-- {{{ Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.setFocussedSpecification SetFocus ]



-- }}}
-- {{{ View


view : Utils.Spa.PageContext -> Model -> Element Msg
view { global } model =
    case global of
        Global.Running { specifications } ->
            case model.focussedSpecification of
                NoFocus ->
                    column
                        [ width fill, spacing 30 ]
                        (row [ centerX, spacing 10 ]
                            [ Style.h1 <|
                                text "Requirement Specifications"
                            , Style.linkButton { url = "/specifications/new", labelText = "New" }
                            ]
                            :: (Dict.toList specifications
                                    |> List.map
                                        (\( k, v ) ->
                                            ( k, Global.storedSpecificationToStub v )
                                        )
                                    |> List.map specificationStubView
                               )
                        )

                Loading ->
                    el [] (text "Loading...")

                Spec uuidString specification ->
                    specificationView uuidString specification

        Global.FailedToLaunch _ ->
            Debug.todo "Add common state page"


specificationStubView : ( String, SpecificationStub ) -> Element Msg
specificationStubView ( uuidString, { name, description, deleteStatus } ) =
    column
        [ padding 15
        , spacing 10
        , width fill
        , Background.color Style.colorPalette.c5
        , Border.rounded 10
        ]
        [ column
            [ pointer
            , spacing 10
            , width fill
            ]
            [ Style.h2 <| text name
            , paragraph [] [ text description ]
            ]
        , row [ spacing 10, width fill ]
            [ Style.alwaysActiveButton
                { labelText = "Details"
                , clickMsg = ClickedFocusSpecification uuidString
                }
            , Style.dangerousButton
                { labelText = "Delete"
                , confirmText = "Are you sure you want to delete this specification?"
                , status = deleteStatus
                , dangerousMsg = DeleteSpecification uuidString
                }
            ]
        ]


specificationView : String -> Specification -> Element Msg
specificationView uuidString { name, description, requirements, deleteStatus } =
    column
        [ padding 15
        , spacing 15
        , width fill
        ]
        [ Style.h1 <| text "Specification Details"
        , row [ spacing 10 ]
            [ Style.alwaysActiveButton
                { labelText = "Back"
                , clickMsg = ClearFocus
                }
            , Style.dangerousButton
                { labelText = "Delete"
                , confirmText = "Are you sure you want to delete this specification?"
                , status = deleteStatus
                , dangerousMsg = DeleteFocussedSpecification uuidString
                }
            ]
        , Style.h2 <| text name
        , paragraph [] [ text description ]
        , Style.h2 <| text "Requirements"
        , requirementView requirements
        ]


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



-- }}}
