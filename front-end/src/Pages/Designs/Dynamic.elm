module Pages.Designs.Dynamic exposing (Model, Msg, page)

import Codec exposing (Value)
import Design exposing (Design, Editable(..))
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import FeatherIcons
import Generated.Designs.Params as Params
import Generated.Routes as Routes exposing (routes)
import Global
import Html
import Html.Attributes as HAtt
import Json.Decode exposing (errorToString)
import Metrics exposing (DesignMetrics)
import Ports
import ReferenceSet
import Round
import Spa.Page exposing (send)
import Specification exposing (Specification)
import Style exposing (h1, h2, h3)
import Utils.Spa exposing (Page)


page : Page Params.Dynamic Model Msg model msg appMsg
page =
    Spa.Page.component
        { title = always "Design Details"
        , init = init
        , update = update
        , subscriptions = always subscriptions
        , view = always view
        }



-- {{{ Init


type alias Model =
    { uuidString : String
    , mSelectedSpecification : Maybe Specification
    , pageState : PageState
    }


type PageState
    = Loading
    | DesignNotFound Codec.Error
    | DesignNoReference Design
    | DesignLoadingReference Design
    | DesignFailedToLoadReference Codec.Error Design
    | DesignWithReference Design


mapPageStateDesign :
    (Design -> Design)
    -> PageState
    -> PageState
mapPageStateDesign designFn focus =
    case focus of
        Loading ->
            focus

        DesignNotFound _ ->
            focus

        DesignNoReference design ->
            design
                |> designFn
                |> DesignNoReference

        DesignFailedToLoadReference errorString design ->
            design
                |> designFn
                |> DesignFailedToLoadReference
                    errorString

        DesignLoadingReference design ->
            design
                |> designFn
                |> DesignLoadingReference

        DesignWithReference design ->
            design
                |> designFn
                |> DesignWithReference


getDesignFromPageState : PageState -> Maybe Design
getDesignFromPageState pageState =
    case pageState of
        Loading ->
            Nothing

        DesignNotFound _ ->
            Nothing

        DesignNoReference design ->
            Just design

        DesignFailedToLoadReference _ design ->
            Just design

        DesignLoadingReference design ->
            Just design

        DesignWithReference design ->
            Just design


init : Utils.Spa.PageContext -> Params.Dynamic -> ( Model, Cmd Msg, Cmd Global.Msg )
init { global } { param1 } =
    ( { uuidString = param1, mSelectedSpecification = Nothing, pageState = Loading }
    , Cmd.none
    , Cmd.batch
        ((Global.GetDesign param1
            |> send
         )
            :: (case global of
                    Global.Running runState ->
                        case runState.mSelectedSpecification of
                            Just uuidString ->
                                [ Codec.encoder Codec.string uuidString
                                    |> Ports.getSpecificationForDesign
                                ]

                            Nothing ->
                                []

                    _ ->
                        []
               )
        )
    )



-- }}}
-- {{{ Update


type Msg
    = SetFocus Value
    | GotReferenceSet Value
    | GotSpecification Value
    | ClickedNameEdit
    | EditedName String
    | ClickedAcceptNameEdit
    | ClickedCancelNameEdit
    | DeleteFocussedDesign String Style.DangerStatus


update : Utils.Spa.PageContext -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update { global } msg model =
    let
        mSelectedReferenceSet =
            case global of
                Global.Running runState ->
                    runState.mSelectedReferenceSet

                _ ->
                    Nothing
    in
    case msg of
        SetFocus value ->
            let
                focusCodec =
                    Codec.object
                        (\uuidString design ->
                            { uuidString = uuidString
                            , design = design
                            }
                        )
                        |> Codec.field "uuidString" .uuidString Codec.string
                        |> Codec.field "design" .design Design.codec
                        |> Codec.buildObject
            in
            case Codec.decodeValue focusCodec value of
                Ok { design } ->
                    case mSelectedReferenceSet of
                        Just refSetId ->
                            ( { model | pageState = DesignLoadingReference design }
                            , Cmd.batch
                                [ Codec.encoder Codec.string design.pdbString
                                    |> Ports.viewStructure
                                , Codec.encoder Codec.string refSetId
                                    |> Ports.getReferenceSetForDesign
                                ]
                            , Cmd.none
                            )

                        Nothing ->
                            ( { model | pageState = DesignNoReference design }
                            , Cmd.batch
                                [ Codec.encoder Codec.string design.pdbString
                                    |> Ports.viewStructure
                                ]
                            , Cmd.none
                            )

                Err error ->
                    ( { model | pageState = DesignNotFound error }
                    , Cmd.none
                    , Cmd.none
                    )

        GotReferenceSet referenceSetValue ->
            let
                refSetWithUuidCodec =
                    Codec.object
                        (\uuidString referenceSet ->
                            { uuidString = uuidString
                            , referenceSet = referenceSet
                            }
                        )
                        |> Codec.field "uuidString" .uuidString Codec.string
                        |> Codec.field "referenceSet" .referenceSet ReferenceSet.codec
                        |> Codec.buildObject
            in
            case Codec.decodeValue refSetWithUuidCodec referenceSetValue of
                Ok { referenceSet } ->
                    case model.pageState of
                        DesignLoadingReference design ->
                            ( { model | pageState = DesignWithReference design }
                            , case design.metricsJobStatus of
                                Ports.Complete metrics ->
                                    Cmd.batch
                                        [ Ports.vegaPlot <|
                                            { plotId = "composition"
                                            , spec =
                                                Metrics.createCompositionSpec
                                                    metrics
                                                    (ReferenceSet.getMetrics referenceSet)
                                            }
                                        , Ports.vegaPlot <|
                                            { plotId = "torsionAngles"
                                            , spec =
                                                Metrics.createTorsionAngleSpec
                                                    metrics
                                                    (ReferenceSet.getMetrics referenceSet)
                                            }
                                        , Ports.vegaPlot <|
                                            { plotId = "metricsHistograms"
                                            , spec =
                                                Metrics.createAllHistogramsSpec
                                                    metrics
                                                    (ReferenceSet.getMetrics referenceSet)
                                            }
                                        ]

                                _ ->
                                    Cmd.none
                            , Cmd.none
                            )

                        _ ->
                            ( model
                            , Cmd.none
                            , Cmd.none
                            )

                Err error ->
                    case model.pageState of
                        DesignLoadingReference design ->
                            ( { model
                                | pageState = DesignFailedToLoadReference error design
                              }
                            , Cmd.none
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none, Cmd.none )

        GotSpecification specificationValue ->
            let
                specWithUuidCodec =
                    Codec.object
                        (\uuidString specification ->
                            { uuidString = uuidString
                            , specification = specification
                            }
                        )
                        |> Codec.field "uuidString" .uuidString Codec.string
                        |> Codec.field "specification" .specification Specification.codec
                        |> Codec.buildObject
            in
            ( { model
                | mSelectedSpecification =
                    Codec.decodeValue specWithUuidCodec specificationValue
                        |> Result.toMaybe
                        |> Maybe.map .specification
              }
            , Cmd.none
            , Cmd.none
            )

        ClickedNameEdit ->
            let
                updateDesignName design =
                    case design.name of
                        NotEditing currentName ->
                            { design | name = Editing currentName <| Just currentName }

                        Editing _ _ ->
                            design
            in
            ( { model
                | pageState = mapPageStateDesign updateDesignName model.pageState
              }
            , Cmd.none
            , Cmd.none
            )

        EditedName newName ->
            let
                updateDesignName design =
                    case design.name of
                        NotEditing _ ->
                            design

                        Editing oldName _ ->
                            { design
                                | name =
                                    if String.isEmpty newName then
                                        Editing oldName Nothing

                                    else
                                        Editing oldName <| Just newName
                            }
            in
            ( { model
                | pageState =
                    mapPageStateDesign
                        updateDesignName
                        model.pageState
              }
            , Cmd.none
            , Cmd.none
            )

        ClickedAcceptNameEdit ->
            let
                updateDesignName design =
                    case design.name of
                        NotEditing _ ->
                            design

                        Editing currentName newName ->
                            { design
                                | name =
                                    Maybe.withDefault currentName newName
                                        |> NotEditing
                            }

                updatedModel =
                    { model
                        | pageState =
                            mapPageStateDesign
                                updateDesignName
                                model.pageState
                    }
            in
            ( updatedModel
            , Cmd.none
            , case getDesignFromPageState updatedModel.pageState of
                Just design ->
                    Global.UpdateFocussedDesign model.uuidString design
                        |> send

                Nothing ->
                    Debug.todo "Catch this"
            )

        ClickedCancelNameEdit ->
            let
                updateDesignName design =
                    case design.name of
                        NotEditing _ ->
                            design

                        Editing currentName _ ->
                            { design | name = NotEditing currentName }
            in
            ( { model
                | pageState =
                    mapPageStateDesign
                        updateDesignName
                        model.pageState
              }
            , Cmd.none
            , Cmd.none
            )

        DeleteFocussedDesign globalUuidString dangerStatus ->
            case dangerStatus of
                Style.Confirmed ->
                    ( model
                    , Cmd.none
                    , Global.DeleteFocussedDesign globalUuidString dangerStatus
                        |> send
                    )

                _ ->
                    ( { model
                        | pageState =
                            mapPageStateDesign
                                (\design ->
                                    { design
                                        | deleteStatus =
                                            dangerStatus
                                    }
                                )
                                model.pageState
                      }
                    , Cmd.none
                    , Cmd.none
                    )



-- }}}
-- {{{ Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.setFocussedDesign SetFocus
        , Ports.referenceSetForDesign GotReferenceSet
        , Ports.specificationForDesign GotSpecification
        ]



-- }}}
-- {{{ View


view : Model -> Element Msg
view model =
    el [ centerX, width <| maximum 800 <| fill ] <|
        case model.pageState of
            Loading ->
                el [] (text "Loading design...")

            DesignNotFound _ ->
                el [] ("A design with ID \"" ++ model.uuidString ++ "\" was not found." |> text)

            DesignNoReference design ->
                sectionColumn
                    [ designDetailsView model.uuidString model.mSelectedSpecification design
                    , text "No reference set selected."
                    ]

            DesignLoadingReference design ->
                sectionColumn
                    [ designDetailsView model.uuidString model.mSelectedSpecification design
                    , h2 <| text "Comparison to Reference Set"
                    , text "Loading reference set..."
                    ]

            DesignFailedToLoadReference error design ->
                sectionColumn
                    [ designDetailsView model.uuidString model.mSelectedSpecification design
                    , h2 <| text "Comparison to Reference Set"
                    , text
                        ("""Comparison to reference set is unavailable, as the reference set
                    data failed to load:"""
                            ++ errorToString error
                        )
                    ]

            DesignWithReference design ->
                sectionColumn
                    [ designDetailsView model.uuidString model.mSelectedSpecification design
                    , referenceSetComparisonView
                    ]


sectionColumn : List (Element msg) -> Element msg
sectionColumn =
    column [ spacing 12, width fill ]


designDetailsView :
    String
    -> Maybe Specification
    -> Design
    -> Element Msg
designDetailsView uuidString mSelectedSpecification design =
    let
        { fileName, deleteStatus, metricsJobStatus } =
            design
    in
    column
        [ spacing 15, width fill ]
        [ sectionColumn
            [ paragraph []
                [ h1 <| text "Design Details" ]
            , row [ height fill, spacing 10 ]
                (case design.name of
                    NotEditing currentName ->
                        [ paragraph [] [ Style.h2 <| text <| "Name: " ++ currentName ]
                        , el [ centerY, Events.onClick <| ClickedNameEdit ]
                            (FeatherIcons.edit
                                |> FeatherIcons.toHtml []
                                |> html
                            )
                        ]

                    Editing _ mNewName ->
                        [ Input.text
                            Style.textInputStyle
                            { onChange = EditedName
                            , text = Maybe.withDefault "" mNewName
                            , placeholder = Nothing
                            , label =
                                Input.labelRight [ centerY ]
                                    (row [ spacing 5 ]
                                        [ Style.conditionalButton
                                            { label = text "Ok"
                                            , clickMsg = ClickedAcceptNameEdit
                                            , isActive =
                                                case mNewName of
                                                    Just newName ->
                                                        String.isEmpty newName
                                                            |> not

                                                    Nothing ->
                                                        False
                                            }
                                        , Style.alwaysActiveButton
                                            { label = text "Cancel"
                                            , clickMsg = ClickedCancelNameEdit
                                            }
                                        ]
                                    )
                            }
                        ]
                )
            , paragraph [] [ text ("Structure file: " ++ fileName) ]
            ]
        , row [ spacing 10 ]
            [ Style.linkButton
                { label = text "Back"
                , url = Routes.toPath routes.designs
                }
            , Style.dangerousButton
                { label = text "Delete"
                , confirmText = "Are you sure you want to delete this design?"
                , status = deleteStatus
                , dangerousMsg = DeleteFocussedDesign uuidString
                }
            ]
        , sectionColumn
            [ h2 <| text "Structure"
            , Keyed.el [ height <| px 400, width fill, padding 5, Border.width 1 ]
                ( "viewer"
                , Html.div
                    [ HAtt.id "viewer"
                    , HAtt.style "height" "100%"
                    , HAtt.style "width" "100%"
                    ]
                    []
                    |> html
                )
            ]
        , case metricsJobStatus of
            Ports.Ready ->
                text "Ready for server submission."

            Ports.Submitted _ ->
                text "Job submitted to server."

            Ports.Queued ->
                text "Job queued on server."

            Ports.InProgress ->
                text "Job is running on server."

            Ports.Cancelled ->
                text "Job was cancelled by user."

            Ports.Failed errorString ->
                "Server error while creating metrics: "
                    ++ errorString
                    |> text

            Ports.Complete designMetrics ->
                basicMetrics mSelectedSpecification designMetrics
        ]


basicMetrics : Maybe Specification -> DesignMetrics -> Element msg
basicMetrics mSelectedSpecification metrics =
    let
        { sequenceInfo } =
            metrics
    in
    sectionColumn
        [ h2 <| text "Basic Metrics"
        , h3 <| text "Sequences and DSSP Assignment"
        , sequenceInfoDictView sequenceInfo
        , metricsOverview metrics
        , case mSelectedSpecification of
            Just specification ->
                specificationView metrics specification

            Nothing ->
                text "No specification selected."
        ]


metricsOverview : DesignMetrics -> Element msg
metricsOverview metrics =
    let
        onePlaceFloatText =
            Round.round 1
                >> text
                >> (\a -> cell a)

        intText =
            String.fromInt >> text >> (\a -> cell a)

        cell =
            el [ centerX, padding 10 ]

        tableColumn =
            column
                [ alignTop
                , width <| px 150
                ]

        headerParagraph =
            paragraph
                [ padding 10
                , height <| px 70
                , Background.color Style.colorPalette.c1
                , Font.center
                , Font.color Style.colorPalette.white
                ]
    in
    column [ spacing 10, width fill ]
        [ h2 <| text "Metrics"
        , wrappedRow
            []
            [ tableColumn
                [ headerParagraph [ text "Hydrophobic Fitness" ]
                , case metrics.hydrophobicFitness of
                    Just hf ->
                        onePlaceFloatText hf

                    Nothing ->
                        text "--"
                ]
            , tableColumn
                [ headerParagraph [ text "pI" ]
                , onePlaceFloatText metrics.isoelectricPoint
                ]
            , tableColumn
                [ headerParagraph [ text "# of Residues" ]
                , intText metrics.numOfResidues
                ]
            , tableColumn
                [ headerParagraph [ text "Mass (Da)" ]
                , onePlaceFloatText metrics.mass
                ]
            , tableColumn
                [ headerParagraph [ text "Mean Packing Density" ]
                , onePlaceFloatText metrics.packingDensity
                ]
            ]
        ]


sequenceInfoDictView : Dict String Metrics.SequenceInfo -> Element msg
sequenceInfoDictView sequenceInfoDict =
    sequenceInfoDict
        |> Dict.toList
        |> List.map sequenceInfoView
        |> column [ padding 15, spacing 5, width fill ]


sequenceInfoView : ( String, Metrics.SequenceInfo ) -> Element msg
sequenceInfoView ( chainId, sequenceInfo ) =
    let
        aaView ( aa, dssp ) =
            column []
                [ String.fromChar aa |> text
                , String.fromChar dssp |> text
                ]

        aaList =
            String.toList sequenceInfo.sequence

        dsspList =
            String.toList sequenceInfo.dsspAssignment
                |> List.map
                    (\c ->
                        if c == ' ' then
                            '-'

                        else
                            c
                    )

        zippedSequenceInfo =
            List.map2 Tuple.pair aaList dsspList
    in
    column [ width fill ]
        [ paragraph [ width fill, Font.bold ] [ "Chain: " ++ chainId |> text ]
        , wrappedRow
            [ width fill
            , spacingXY 0 10
            , Font.family
                [ Font.typeface "Roboto Mono"
                , Font.monospace
                ]
            , Font.size 14
            ]
            (List.map aaView zippedSequenceInfo)
        ]


referenceSetComparisonView : Element msg
referenceSetComparisonView =
    sectionColumn
        [ h2 <| text "Comparison to Reference Set"
        , compositionView
        , torsionAnglesView
        , metricsHistogramsView
        ]


compositionView : Element msg
compositionView =
    column
        [ width fill ]
        [ h3 <| text "Composition"
        , Keyed.el [ centerX, width fill ]
            ( "composition"
            , Html.div
                [ HAtt.id "composition"
                , HAtt.style "width" "100%"
                ]
                [ Html.div
                    [ HAtt.height 200
                    , HAtt.style "height" "200px"
                    , HAtt.style "width" "100%"
                    , HAtt.style "border-radius" "5px"
                    , HAtt.style "background-color" "#d3d3d3"
                    ]
                    []
                ]
                |> html
            )
        ]


torsionAnglesView : Element msg
torsionAnglesView =
    column
        [ centerX ]
        [ h3 <| text "Backbone Torsion Angles"
        , Keyed.el []
            ( "torsionAngles"
            , Html.div
                [ HAtt.id "torsionAngles"
                , HAtt.style "width" "100%"
                ]
                [ Html.div
                    [ HAtt.height 200
                    , HAtt.style "height" "200px"
                    , HAtt.style "width" "100%"
                    , HAtt.style "border-radius" "5px"
                    , HAtt.style "background-color" "#d3d3d3"
                    ]
                    []
                ]
                |> html
            )
        ]


metricsHistogramsView : Element msg
metricsHistogramsView =
    column [ width fill ]
        [ h3 <| text "Metrics Histograms"
        , Keyed.el [ centerX ]
            ( "metricsHistograms"
            , Html.div
                [ HAtt.id "metricsHistograms"
                ]
                [ Html.div
                    [ HAtt.height 200
                    , HAtt.style "height" "200px"
                    , HAtt.style "width" "100%"
                    , HAtt.style "border-radius" "5px"
                    , HAtt.style "background-color" "#d3d3d3"
                    ]
                    []
                ]
                |> html
            )
        ]



-- {{{ specificationView


specificationView : DesignMetrics -> Specification -> Element msg
specificationView metrics { name, description, requirements } =
    sectionColumn
        [ h2 <| text "Specification Evaluation"
        , column
            [ padding 15
            , spacing 10
            , width fill
            , Background.color Style.colorPalette.c5
            , Border.rounded 10
            ]
            [ h3 <| text "Name"
            , paragraph [] [ text name ]
            , h3 <| text "Description"
            , paragraph [] [ text description ]
            , h3 <| text "Requirements"
            , requirementView metrics requirements
            ]
        ]


requirementView :
    DesignMetrics
    -> Specification.Requirement Specification.RequirementData
    -> Element msg
requirementView metrics requirement =
    let
        requirementResolves =
            Specification.resolveRequirement metrics requirement

        arrowRow r =
            row
                [ padding 5, spacing 15, width fill ]
                [ el []
                    (FeatherIcons.chevronRight
                        |> FeatherIcons.toHtml []
                        |> html
                    )
                , requirementView metrics r
                ]
    in
    el
        ([ width fill, Border.rounded 10 ]
            ++ (if requirementResolves then
                    [ Background.color Style.colorPalette.c3 ]

                else
                    [ Background.color Style.colorPalette.red ]
               )
        )
    <|
        case requirement of
            Specification.Data data ->
                case data of
                    Specification.Constant constantType ->
                        let
                            typeString =
                                "Constant:"

                            requirementString =
                                case constantType of
                                    Specification.Method methodType ->
                                        let
                                            constantTypeString =
                                                typeString ++ "Method:"
                                        in
                                        case methodType of
                                            Specification.SPPS ->
                                                constantTypeString ++ "SPPS"

                                            Specification.MolecularBiology ->
                                                constantTypeString ++ "MolBio"
                        in
                        el [ padding 10 ] (text <| requirementString)

                    Specification.Value valueType ->
                        let
                            typeString =
                                "Value:"

                            requirementString =
                                case valueType of
                                    Specification.IsoelectricPoint order value ->
                                        typeString
                                            ++ "IsoelectricPoint:"
                                            ++ Specification.stringFromOrder
                                                order
                                            ++ ":"
                                            ++ String.fromFloat value

                                    Specification.HydrophobicFitness order value ->
                                        typeString
                                            ++ "HydrophobicFitness:"
                                            ++ Specification.stringFromOrder
                                                order
                                            ++ ":"
                                            ++ String.fromFloat value

                                    Specification.MeanPackingDensity order value ->
                                        typeString
                                            ++ "MeanPackingDensity:"
                                            ++ Specification.stringFromOrder
                                                order
                                            ++ ":"
                                            ++ String.fromFloat value

                                    Specification.SequenceContains string ->
                                        typeString
                                            ++ "SequenceContains:"
                                            ++ string
                        in
                        el (Style.defaultBorder ++ [ padding 10, width fill ])
                            (text <| requirementString)

            Specification.Not subRequirement ->
                row (Style.defaultBorder ++ [ padding 10, spacing 10, width fill ])
                    [ h3 <| el [ Font.bold ] (text <| "NOT")
                    , requirementView metrics subRequirement
                    ]

            Specification.Or subRequirement1 subRequirement2 ->
                column
                    (Style.defaultBorder
                        ++ [ padding 10
                           , spacing 10
                           , width fill
                           ]
                    )
                    [ requirementView metrics subRequirement1
                    , h3 <| el [ Font.bold ] (text "---- OR ----")
                    , requirementView metrics subRequirement2
                    ]

            Specification.And requirement1 requirement2 ->
                column
                    (Style.defaultBorder
                        ++ [ padding 10
                           , spacing 10
                           , width fill
                           ]
                    )
                    [ requirementView metrics requirement1
                    , el [ Font.bold ]
                        (text "---- AND ----")
                    , requirementView metrics requirement2
                    ]

            Specification.Any requirements ->
                column
                    (Style.defaultBorder
                        ++ [ padding 10
                           , spacing 10
                           , width fill
                           ]
                    )
                    [ el [ Font.bold ] (text "ANY")
                    , column [ padding 10, spacing 10, width fill ] <|
                        List.map arrowRow requirements
                    ]

            Specification.All requirements ->
                column
                    (Style.defaultBorder
                        ++ [ padding 10
                           , spacing 10
                           , width fill
                           ]
                    )
                    [ el [ Font.bold ] (text "ALL")
                    , column [ padding 10, spacing 5, width fill ] <|
                        List.map arrowRow requirements
                    ]



-- }}}
-- }}}
