module Pages.Designs.Dynamic exposing (Model, Msg, page)

import Codec exposing (Value)
import Design exposing (Design)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Keyed as Keyed
import Generated.Designs.Params as Params
import Generated.Routes as Routes exposing (routes)
import Global
import Html
import Html.Attributes as HAtt
import Json.Decode exposing (errorToString)
import Metrics exposing (DesignMetrics)
import Ports
import ReferenceSet
import RemoteData as RD
import Round
import Spa.Page exposing (send)
import Style exposing (h1, h2, h3)
import Utils.Spa exposing (Page)


page : Page Params.Dynamic Model Msg model msg appMsg
page =
    Spa.Page.component
        { title = always "Design Details"
        , init = always init
        , update = update
        , subscriptions = always subscriptions
        , view = always view
        }



-- {{{ Init


type Model
    = Loading String
    | DesignNotFound String Codec.Error
    | DesignNoReference String Design
    | DesignLoadingReference String Design
    | DesignFailedToLoadReference String Design Codec.Error
    | DesignWithReference String Design


getUuidString : Model -> String
getUuidString model =
    case model of
        Loading uuidString ->
            uuidString

        DesignNotFound uuidString _ ->
            uuidString

        DesignNoReference uuidString _ ->
            uuidString

        DesignLoadingReference uuidString _ ->
            uuidString

        DesignFailedToLoadReference uuidString _ _ ->
            uuidString

        DesignWithReference uuidString _ ->
            uuidString


mapDesign :
    ({ uuidString : String, design : Design }
     -> { uuidString : String, design : Design }
    )
    -> Model
    -> Model
mapDesign designFn focus =
    case focus of
        Loading _ ->
            focus

        DesignNotFound _ _ ->
            focus

        DesignNoReference uuidString design ->
            { uuidString = uuidString, design = design }
                |> designFn
                |> (\idDesign -> DesignNoReference idDesign.uuidString idDesign.design)

        DesignFailedToLoadReference uuidString design errorString ->
            { uuidString = uuidString, design = design }
                |> designFn
                |> (\idDesign ->
                        DesignFailedToLoadReference
                            idDesign.uuidString
                            idDesign.design
                            errorString
                   )

        DesignLoadingReference uuidString design ->
            { uuidString = uuidString, design = design }
                |> designFn
                |> (\idDesign -> DesignLoadingReference idDesign.uuidString idDesign.design)

        DesignWithReference uuidString design ->
            { uuidString = uuidString, design = design }
                |> designFn
                |> (\idDesign ->
                        DesignWithReference idDesign.uuidString
                            idDesign.design
                   )


init : Params.Dynamic -> ( Model, Cmd Msg, Cmd Global.Msg )
init { param1 } =
    ( Loading param1
    , Cmd.none
    , Global.GetDesign param1 |> send
    )



-- }}}
-- {{{ Update


type Msg
    = SetFocus Value
    | GotReferenceSet Value
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
                Ok { uuidString, design } ->
                    case mSelectedReferenceSet of
                        Just refSetId ->
                            ( DesignLoadingReference uuidString design
                            , Cmd.batch
                                [ Codec.encoder Codec.string design.pdbString
                                    |> Ports.viewStructure
                                , Codec.encoder Codec.string refSetId
                                    |> Ports.getReferenceSetForMetrics
                                ]
                            , Cmd.none
                            )

                        Nothing ->
                            ( DesignNoReference uuidString design
                            , Cmd.batch
                                [ Codec.encoder Codec.string design.pdbString
                                    |> Ports.viewStructure
                                ]
                            , Cmd.none
                            )

                Err error ->
                    ( DesignNotFound (getUuidString model) error
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
                    case model of
                        DesignLoadingReference uuidString design ->
                            ( DesignWithReference uuidString design
                            , case design.metricsRemoteData of
                                RD.Success metrics ->
                                    Ports.vegaPlot <|
                                        { plotId = "composition"
                                        , spec =
                                            Metrics.createCompositionSpec
                                                metrics
                                                (ReferenceSet.getMetrics referenceSet)
                                        }

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
                    case model of
                        DesignLoadingReference uuidString design ->
                            ( DesignFailedToLoadReference uuidString design error
                            , Cmd.none
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none, Cmd.none )

        DeleteFocussedDesign globalUuidString dangerStatus ->
            case dangerStatus of
                Style.Confirmed ->
                    ( model
                    , Cmd.none
                    , Global.DeleteFocussedDesign globalUuidString dangerStatus
                        |> send
                    )

                _ ->
                    ( mapDesign
                        (\{ uuidString, design } ->
                            { uuidString = uuidString
                            , design =
                                { design
                                    | deleteStatus =
                                        dangerStatus
                                }
                            }
                        )
                        model
                    , Cmd.none
                    , Cmd.none
                    )



-- }}}
-- {{{ Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.setFocussedDesign SetFocus
        , Ports.referenceSetForMetrics GotReferenceSet
        ]



-- }}}
-- {{{ View


view : Model -> Element Msg
view model =
    case model of
        Loading _ ->
            el [] (text "Loading design...")

        DesignNotFound designUuid _ ->
            el [] ("A design with ID \"" ++ designUuid ++ "\" was not found." |> text)

        DesignNoReference uuidString design ->
            column
                [ spacing 15, width fill ]
                [ designDetailsView uuidString design
                , text "No reference set selected."
                ]

        DesignLoadingReference uuidString design ->
            column
                [ spacing 15, width fill ]
                [ designDetailsView uuidString design
                , h2 <| text "Comparison to Reference Set"
                , text "Loading reference set..."
                ]

        DesignFailedToLoadReference uuidString design error ->
            column
                [ spacing 15, width fill ]
                [ designDetailsView uuidString design
                , h2 <| text "Comparison to Reference Set"
                , text
                    ("""Comparison to reference set is unavailable, as the reference set
                    data failed to load:"""
                        ++ errorToString error
                    )
                ]

        DesignWithReference uuidString design ->
            column
                [ spacing 15, width fill ]
                [ designDetailsView uuidString design
                , referenceSetComparisonView
                ]


sectionColumn : List (Element msg) -> Element msg
sectionColumn =
    column [ spacing 10, width fill ]


designDetailsView : String -> Design -> Element Msg
designDetailsView uuidString { name, fileName, deleteStatus, metricsRemoteData } =
    column
        [ spacing 15, width fill ]
        [ sectionColumn
            [ paragraph [ centerX ]
                [ h1 <| text (Design.editableValue name ++ " Design Details") ]
            , paragraph [] [ text ("Structure file: " ++ fileName) ]
            ]
        , row [ spacing 10 ]
            [ Style.linkButton
                { labelText = "Back"
                , url = Routes.toPath routes.designs
                }
            , Style.dangerousButton
                { labelText = "Delete"
                , confirmText = "Are you sure you want to delete this design?"
                , status = deleteStatus
                , dangerousMsg = DeleteFocussedDesign uuidString
                }
            ]
        , sectionColumn
            [ h2 <| text "Structure"
            , Keyed.el [ height <| px 300, width fill, Border.width 1 ]
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
        , Metrics.desMetRemoteDataView basicMetrics metricsRemoteData
        ]


basicMetrics : DesignMetrics -> Element msg
basicMetrics metrics =
    let
        { sequences } =
            metrics
    in
    sectionColumn
        [ h2 <| text "Basic Metrics"
        , h3 <| text "Sequences"
        , sequenceDictView sequences
        , metricsOverview metrics
        ]


referenceSetComparisonView : Element msg
referenceSetComparisonView =
    sectionColumn
        [ h2 <| text "Comparison to Reference Set"
        , compositionView
        ]


compositionView : Element msg
compositionView =
    column
        [ width fill ]
        [ h3 <| text "Composition"
        , Keyed.el [ centerX ]
            ( "composition"
            , Html.div [ HAtt.id "composition" ] []
                |> html
            )
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


sequenceDictView : Dict String String -> Element msg
sequenceDictView sequenceDict =
    sequenceDict
        |> Dict.toList
        |> List.map sequenceView
        |> column [ padding 15, spacing 5, width fill ]


sequenceView : ( String, String ) -> Element msg
sequenceView ( chainId, sequence ) =
    let
        aaView char =
            char
                |> String.fromChar
                |> text
                |> el []
    in
    column [ width fill ]
        [ paragraph [ width fill, Font.bold ] [ "Chain: " ++ chainId |> text ]
        , sequence
            |> String.toList
            |> List.map aaView
            |> wrappedRow
                [ width fill
                , Font.family
                    [ Font.typeface "Roboto Mono"
                    , Font.monospace
                    ]
                ]
        ]



-- }}}
