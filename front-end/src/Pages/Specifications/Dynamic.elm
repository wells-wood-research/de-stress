module Pages.Specifications.Dynamic exposing (Model, Msg, page, requirementView)

import Codec exposing (Value)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import FeatherIcons
import Generated.Routes as Routes exposing (routes)
import Generated.Specifications.Params as Params
import Global
import Ports
import Spa.Page exposing (send)
import Specification as Spec exposing (Specification)
import Style
import Utils.Spa exposing (Page)


page : Page Params.Dynamic Model Msg model msg appMsg
page =
    Spa.Page.component
        { title = always "Specification Details"
        , init = always init
        , update = always update
        , subscriptions = always subscriptions
        , view = always view
        }



-- {{{ Init


type Model
    = Loading String
    | SpecNotFound String
    | Spec String Specification


mapModel :
    ({ uuidString : String, specification : Specification }
     -> { uuidString : String, specification : Specification }
    )
    -> Model
    -> Model
mapModel specFn focus =
    case focus of
        Loading _ ->
            focus

        SpecNotFound _ ->
            focus

        Spec uuidString specification ->
            { uuidString = uuidString, specification = specification }
                |> specFn
                |> (\idSpec -> Spec idSpec.uuidString idSpec.specification)


init : Params.Dynamic -> ( Model, Cmd Msg, Cmd Global.Msg )
init { param1 } =
    ( Loading param1
    , Cmd.none
    , Global.GetSpecification param1
        |> send
    )



-- }}}
-- UPDATE


type Msg
    = SetFocus Value
    | DeleteFocussedSpecification String Style.DangerStatus


update : Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update msg model =
    case msg of
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
                    ( Spec uuidString specification
                    , Cmd.none
                    , Cmd.none
                    )

                Err errorString ->
                    Debug.todo "Catch this error"

        DeleteFocussedSpecification globalUuidString dangerStatus ->
            case dangerStatus of
                Style.Confirmed ->
                    ( model
                    , Cmd.none
                    , Global.DeleteFocussedSpecification globalUuidString dangerStatus
                        |> send
                    )

                _ ->
                    ( mapModel
                        (\{ uuidString, specification } ->
                            { uuidString = uuidString
                            , specification =
                                { specification
                                    | deleteStatus =
                                        dangerStatus
                                }
                            }
                        )
                        model
                    , Cmd.none
                    , Cmd.none
                    )



-- {{{ Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.setFocussedSpecification SetFocus ]



-- }}}
-- VIEW


view : Model -> Element Msg
view model =
    case model of
        Loading _ ->
            el [] (text "Loading specification...")

        SpecNotFound specificationUuid ->
            el [] ("A specification with ID \"" ++ specificationUuid ++ "\" was not found." |> text)

        Spec uuidString specification ->
            specificationDetailsView uuidString specification


specificationDetailsView : String -> Specification -> Element Msg
specificationDetailsView uuidString { name, description, requirements, deleteStatus } =
    column
        [ padding 15
        , spacing 15
        , width fill
        ]
        [ Style.h1 <| text "Specification Details"
        , row [ spacing 10 ]
            [ Style.linkButton
                { labelText = "Back"
                , url = Routes.toPath routes.specifications
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
