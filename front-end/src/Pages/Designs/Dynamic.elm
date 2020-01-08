module Pages.Designs.Dynamic exposing (Model, Msg, page)

import Codec exposing (Value)
import Design exposing (Design)
import Element exposing (..)
import Element.Keyed as Keyed
import Generated.Designs.Params as Params
import Generated.Routes as Routes exposing (routes)
import Global
import Html
import Html.Attributes as HAtt
import Ports
import Spa.Page exposing (send)
import Style exposing (h1, h2)
import Utils.Spa exposing (Page)


page : Page Params.Dynamic Model Msg model msg appMsg
page =
    Spa.Page.component
        { title = always "Design Details"
        , init = always init
        , update = always update
        , subscriptions = always subscriptions
        , view = always view
        }



-- {{{ Init


type Model
    = Loading String
    | DesignNotFound String
    | Design String Design


mapModel :
    ({ uuidString : String, design : Design }
     -> { uuidString : String, design : Design }
    )
    -> Model
    -> Model
mapModel designFn focus =
    case focus of
        Loading _ ->
            focus

        DesignNotFound _ ->
            focus

        Design uuidString design ->
            { uuidString = uuidString, design = design }
                |> designFn
                |> (\idDesign -> Design idDesign.uuidString idDesign.design)


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
    | DeleteFocussedDesign String Style.DangerStatus


update : Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update msg model =
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
                    ( Design uuidString design
                    , Codec.encoder Codec.string design.pdbString
                        |> Ports.viewStructure
                    , Cmd.none
                    )

                Err errorString ->
                    Debug.todo "Catch this error"

        DeleteFocussedDesign globalUuidString dangerStatus ->
            case dangerStatus of
                Style.Confirmed ->
                    ( model
                    , Cmd.none
                    , Global.DeleteFocussedDesign globalUuidString dangerStatus
                        |> send
                    )

                _ ->
                    ( mapModel
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
        [ Ports.setFocussedDesign SetFocus ]



-- }}}
-- {{{ View


view : Model -> Element Msg
view model =
    case model of
        Loading _ ->
            el [] (text "Loading design...")

        DesignNotFound designUuid ->
            el [] ("A design with ID \"" ++ designUuid ++ "\" was not found." |> text)

        Design uuidString design ->
            designDetailsView uuidString design


designDetailsView : String -> Design -> Element Msg
designDetailsView uuidString { name, fileName, deleteStatus } =
    let
        sectionColumn =
            column [ spacing 10, width fill ]
    in
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
            , Keyed.el [ height <| px 300, width fill ]
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
        , sectionColumn
            [ h2 <| text "Sequences"
            ]

        -- , metricsView designMetrics
        -- , compareToPdb designMetrics referenceSetMetrics
        -- , case mSpecification of
        --     Nothing ->
        --         none
        --     Just specification ->
        --         sectionColumn
        --             [ Common.h2 <| text "Active Requirement Specification"
        --             , specificationView sequenceStrings designMetrics specification
        --             ]
        ]



-- }}}
