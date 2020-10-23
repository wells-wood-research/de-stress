port module Pages.Designs.Uuid_String exposing (Model, Msg, Params, page)

import Codec exposing (Value)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import FeatherIcons
import Html
import Html.Attributes as HAtt
import Round
import Shared
import Shared.Buttons as Buttons
import Shared.Design as Design
import Shared.Editable exposing (Editable(..))
import Shared.Metrics as Metrics
import Shared.Requirement as Requirement exposing (Requirement, RequirementData)
import Shared.Specification as Specification exposing (Specification)
import Shared.Style as Style
import Shared.WebSockets as WebSockets
import Spa.Document exposing (Document)
import Spa.Generated.Route as Route
import Spa.Page as Page exposing (Page)
import Spa.Url exposing (Url)


page : Page Params Model Msg
page =
    Page.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , save = save
        , load = load
        }



-- {{{ PORTS


port setFocussedDesign :
    ({ uuidString : String, design : Value } -> msg)
    -> Sub msg


port setSelectedSpecDesignDetails :
    ({ uuidString : String, specValue : Value } -> msg)
    -> Sub msg



-- }}}
-- {{{ MODEL


type alias Model =
    { designUuid : String
    , mSelectedSpecUuid : Maybe String
    , pageState : PageState
    }


type PageState
    = AppNotRunning
    | LoadingNoStub
    | LoadingWithStub Design.DesignStub
    | UnknownDesignUuid
    | Design Design.Design
    | DesignFailedSpec Design.Design
    | DesignWithSpec Design.Design Specification



-- }}}
-- {{{ INIT


type alias Params =
    { uuid : String }


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared { params } =
    case Shared.getRunState shared of
        Just runState ->
            ( { designUuid = params.uuid
              , mSelectedSpecUuid = runState.mSelectedSpecification
              , pageState =
                    case
                        Dict.get params.uuid runState.designs
                            |> Maybe.map Design.storedDesignToStub
                    of
                        Just stub ->
                            LoadingWithStub stub

                        Nothing ->
                            LoadingNoStub
              }
            , Design.getStoredDesign { uuidString = params.uuid }
            )

        Nothing ->
            ( { designUuid = params.uuid
              , mSelectedSpecUuid = Nothing
              , pageState = AppNotRunning
              }
            , Cmd.none
            )



-- }}}
-- {{{ UPDATE


type Msg
    = SetFocus { uuidString : String, design : Value }
    | SetSpecification { uuidString : String, specValue : Value }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetFocus { design } ->
            case Codec.decodeValue Design.codec design of
                Ok des ->
                    ( { model | pageState = Design des }
                    , Cmd.batch
                        [ Design.viewStructure des.pdbString
                        , case model.mSelectedSpecUuid of
                            Just uuidString ->
                                Specification.getSpecificationForSpecDetails
                                    { uuidString = uuidString }

                            Nothing ->
                                Cmd.none
                        ]
                    )

                Err _ ->
                    ( { model | pageState = UnknownDesignUuid }
                    , Cmd.none
                    )

        SetSpecification { specValue } ->
            ( { model
                | pageState =
                    case model.pageState of
                        Design design ->
                            case Codec.decodeValue Specification.codec specValue of
                                Ok specification ->
                                    DesignWithSpec design specification

                                Err _ ->
                                    DesignFailedSpec design

                        _ ->
                            Debug.log "Unexpected page state"
                                model.pageState
              }
            , Cmd.none
            )


save : Model -> Shared.Model -> Shared.Model
save model shared =
    shared


load : Shared.Model -> Model -> ( Model, Cmd Msg )
load shared model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ setFocussedDesign SetFocus
        , setSelectedSpecDesignDetails SetSpecification
        ]



-- }}}
-- {{{ VIEW


view : Model -> Document Msg
view model =
    { title = "Design Details"
    , body = [ bodyView model ]
    }


sectionColumn : List (Element msg) -> Element msg
sectionColumn =
    column [ spacing 12, width fill ]


bodyView : Model -> Element Msg
bodyView { designUuid, pageState } =
    column [ width fill ]
        [ el [ centerX ] (Style.h1 <| text "Design Details")
        , case pageState of
            AppNotRunning ->
                sectionColumn
                    [ paragraph []
                        [ """An error has occurred while initialising the
                            application. In fact, you should never really be able to see
                            this message as the application should not get this far. Try
                            closing your browser and visiting the site again, or try a
                            different browser.
                            """
                            |> text
                        ]
                    ]

            LoadingNoStub ->
                sectionColumn [ text "Loading..." ]

            LoadingWithStub stub ->
                basicInformation stub

            UnknownDesignUuid ->
                sectionColumn
                    [ paragraph []
                        [ "Failed to load a design with UUID: "
                            |> text
                        , el [ Font.bold ] (text designUuid)
                        ]
                    , paragraph []
                        [ """This design no longer exists, you might have deleted
                            it or it may be stored on another computer. DESTRESS does
                            not store your designs on our server by default. If you'd
                            like this behaviour, you can opt in using the settings
                            panel.
                            """
                            |> text
                        ]
                    ]

            Design design ->
                sectionColumn
                    [ designDetailsView designUuid Nothing design ]

            DesignFailedSpec design ->
                sectionColumn
                    [ "FAILED TO LOAD SPECIFICATION"
                        |> Debug.log "Design page"
                            text
                    , designDetailsView designUuid Nothing design
                    ]

            DesignWithSpec design specification ->
                sectionColumn
                    [ designDetailsView
                        designUuid
                        (Just specification)
                        design
                    ]
        ]


basicInformation : { stubOrDesign | name : String, fileName : String } -> Element msg
basicInformation { name, fileName } =
    column
        [ width fill
        ]
        [ Style.h2 <| text ("Design Name: " ++ name)
        , Style.h3 <| text ("File: " ++ fileName)
        ]


designDetailsView :
    String
    -> Maybe Specification
    -> Design.Design
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
                [ Style.h1 <| text "Design Details" ]
            , row [ height fill, spacing 10 ]
                (case design.name of
                    NotEditing currentName ->
                        [ paragraph [] [ Style.h2 <| text <| "Name: " ++ currentName ]
                        , el [ centerY ]
                            --, Events.onClick <| ClickedNameEdit ]
                            (FeatherIcons.edit
                                |> FeatherIcons.toHtml []
                                |> html
                            )
                        ]

                    Editing _ mNewName ->
                        -- [ Input.text
                        --     Style.textInputStyle
                        --     { onChange = EditedName
                        --     , text = Maybe.withDefault "" mNewName
                        --     , placeholder = Nothing
                        --     , label =
                        --         Input.labelRight [ centerY ]
                        --             (row [ spacing 5 ]
                        --                 [ Style.conditionalButton
                        --                     { label = text "Ok"
                        --                     , clickMsg = Just ClickedAcceptNameEdit
                        --                     , isActive =
                        --                         case mNewName of
                        --                             Just newName ->
                        --                                 String.isEmpty newName
                        --                                     |> not
                        --                             Nothing ->
                        --                                 False
                        --                     }
                        --                 , Style.alwaysActiveButton
                        --                     { label = text "Cancel"
                        --                     , clickMsg = ClickedCancelNameEdit
                        --                     , pressed = False
                        --                     }
                        --                 ]
                        --             )
                        --     }
                        []
                )
            , paragraph [] [ text ("Structure file: " ++ fileName) ]
            ]
        , row [ spacing 10 ]
            [ Buttons.linkButton
                { label = text "Back"
                , route = Route.Designs
                }

            -- , Buttons.dangerousButton
            --     { label = text "Delete"
            --     , confirmText = "Are you sure you want to delete this design?"
            --     , status = deleteStatus
            --     , dangerousMsg = DeleteFocussedDesign uuidString
            --     }
            ]
        , sectionColumn
            [ Style.h2 <| text "Structure"
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
        , case WebSockets.getDesignMetrics metricsJobStatus of
            Just designMetrics ->
                basicMetrics mSelectedSpecification designMetrics

            Nothing ->
                WebSockets.metricsJobStatusString metricsJobStatus
                    |> text
        ]


basicMetrics : Maybe Specification -> Metrics.DesignMetrics -> Element msg
basicMetrics mSelectedSpecification metrics =
    let
        { sequenceInfo } =
            metrics
    in
    sectionColumn
        [ Style.h2 <| text "Basic Metrics"
        , Style.h3 <| text "Sequences and DSSP Assignment"
        , sequenceInfoDictView sequenceInfo
        , metricsOverview metrics
        , case mSelectedSpecification of
            Just specification ->
                specificationView metrics specification

            Nothing ->
                text "No specification selected."
        ]


metricsOverview : Metrics.DesignMetrics -> Element msg
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
        [ Style.h2 <| text "Metrics"
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
        [ Style.h2 <| text "Comparison to Reference Set"
        , compositionView
        , torsionAnglesView
        , metricsHistogramsView
        ]


compositionView : Element msg
compositionView =
    column
        [ width fill ]
        [ Style.h3 <| text "Composition"
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
        [ Style.h3 <| text "Backbone Torsion Angles"
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
        [ Style.h3 <| text "Metrics Histograms"
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


specificationView : Metrics.DesignMetrics -> Specification -> Element msg
specificationView metrics { name, description, requirements } =
    sectionColumn
        [ Style.h2 <| text "Specification Evaluation"
        , column
            [ padding 15
            , spacing 10
            , width fill
            , Background.color Style.colorPalette.c5
            , Border.rounded 10
            ]
            [ Style.h3 <| text "Name"
            , paragraph [] [ text name ]
            , Style.h3 <| text "Description"
            , paragraph [] [ text description ]
            , Style.h3 <| text "Requirements"
            , requirementView metrics requirements
            ]
        ]


requirementView :
    Metrics.DesignMetrics
    -> Requirement RequirementData
    -> Element msg
requirementView metrics requirement =
    let
        requirementResolves =
            Requirement.resolveRequirement Nothing metrics requirement

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
            Requirement.Data data ->
                case data of
                    Requirement.Constant constantType ->
                        let
                            typeString =
                                "Constant:"

                            requirementString =
                                case constantType of
                                    Requirement.Method methodType ->
                                        let
                                            constantTypeString =
                                                typeString ++ "Method:"
                                        in
                                        case methodType of
                                            Requirement.SPPS ->
                                                constantTypeString ++ "SPPS"

                                            Requirement.MolecularBiology ->
                                                constantTypeString ++ "MolBio"
                        in
                        el [ padding 10 ] (text <| requirementString)

                    Requirement.Value valueType ->
                        let
                            typeString =
                                "Value:"

                            requirementString =
                                case valueType of
                                    Requirement.IsoelectricPoint order value ->
                                        typeString
                                            ++ "IsoelectricPoint:"
                                            ++ Requirement.stringFromOrder
                                                order
                                            ++ ":"
                                            ++ String.fromFloat value

                                    Requirement.HydrophobicFitness order value ->
                                        typeString
                                            ++ "HydrophobicFitness:"
                                            ++ Requirement.stringFromOrder
                                                order
                                            ++ ":"
                                            ++ String.fromFloat value

                                    Requirement.MeanPackingDensity order value ->
                                        typeString
                                            ++ "MeanPackingDensity:"
                                            ++ Requirement.stringFromOrder
                                                order
                                            ++ ":"
                                            ++ String.fromFloat value

                                    Requirement.SequenceContains string ->
                                        typeString
                                            ++ "SequenceContains:"
                                            ++ string

                                    Requirement.CompositionDeviation unitType value ->
                                        typeString
                                            ++ "CompositionDeviation:"
                                            ++ Requirement.stringFromUnitType unitType
                                            ++ ":"
                                            ++ String.fromFloat value
                        in
                        el (Style.defaultBorder ++ [ padding 10, width fill ])
                            (text <| requirementString)

            Requirement.Not subRequirement ->
                row (Style.defaultBorder ++ [ padding 10, spacing 10, width fill ])
                    [ Style.h3 <| el [ Font.bold ] (text <| "NOT")
                    , requirementView metrics subRequirement
                    ]

            Requirement.Or subRequirement1 subRequirement2 ->
                column
                    (Style.defaultBorder
                        ++ [ padding 10
                           , spacing 10
                           , width fill
                           ]
                    )
                    [ requirementView metrics subRequirement1
                    , Style.h3 <| el [ Font.bold ] (text "---- OR ----")
                    , requirementView metrics subRequirement2
                    ]

            Requirement.And requirement1 requirement2 ->
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

            Requirement.Any requirements ->
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

            Requirement.All requirements ->
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
