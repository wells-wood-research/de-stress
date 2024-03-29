module Pages.Specifications.New exposing (Model, Msg, Params, page)

import Browser.Navigation as Nav
import Codec
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Shared
import Shared.Buttons as Buttons
import Shared.Error as Error
import Shared.Requirement as Requirement
    exposing
        ( Requirement
        , RequirementData
        , UnitType(..)
        , ValueType(..)
        )
import Shared.ResourceUuid as ResourceUuid exposing (ResourceUuid)
import Shared.Specification as Specification
import Shared.Style as Style
import Spa.Document exposing (Document)
import Spa.Generated.Route as Route
import Spa.Page as Page exposing (Page)
import Spa.Url exposing (Url)
import Utils.Route exposing (navigate)


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



-- {{{ MODEL


type alias Model =
    { mResourceUuid : Maybe ResourceUuid
    , specifications : Dict String Specification.StoredSpecification
    , requirements : List (Requirement RequirementData)
    , name : Maybe String
    , description : Maybe String
    , mode : Mode
    , pageErrors : List Error.Error
    , navKey : Nav.Key
    }


defaultModel : Nav.Key -> Model
defaultModel navKey =
    { mResourceUuid = Nothing
    , specifications = Dict.empty
    , requirements = []
    , name = Nothing
    , description = Nothing
    , mode = View
    , pageErrors = []
    , navKey = navKey
    }


type Mode
    = View
    | Add (Maybe (NewRequirement NewRequirementData))



-- {{{ NewRequirement


type NewRequirement a
    = Data a
    | Not (Maybe (NewRequirement a))
    | Or (Maybe (NewRequirement a)) (Maybe (NewRequirement a))
    | And (Maybe (NewRequirement a)) (Maybe (NewRequirement a))


type NewRequirementData
    = Constant (Maybe ConstantType)
    | Value (Maybe ValueType)


type ConstantType
    = Method (Maybe MethodType)


stringFromConstantType : ConstantType -> String
stringFromConstantType constantType =
    case constantType of
        Method _ ->
            "Method"


type MethodType
    = SPPS
    | MolecularBiology


stringFromMethodType : MethodType -> String
stringFromMethodType methodType =
    case methodType of
        SPPS ->
            "SPPS"

        MolecularBiology ->
            "Molecular Biology"


stringFromValueType : ValueType -> String
stringFromValueType valueType =
    case valueType of
        IsoelectricPoint _ _ ->
            "Isoelectric Point"

        HydrophobicFitness _ _ ->
            "Hydrophobic Fitness"

        MeanPackingDensity _ _ ->
            "Mean Packing Density"

        SequenceContains _ ->
            "Sequence Contains"

        CompositionDeviation _ _ ->
            "Composition Deviation"

        BUDEFFTotal _ _ ->
            "BUDEFF Total Energy"

        EvoEF2Total _ _ ->
            "EvoEF2 Total Energy"

        DFIRE2Total _ _ ->
            "DFIRE2 Total Energy"

        RosettaTotal _ _ ->
            "Rosetta Total Energy"

        Agg3DTotal _ _ ->
            "Aggrescan3D Total Score"



-- Consider unifying to use relative value to replace order definition


type ValueType
    = IsoelectricPoint (Maybe Order) (Maybe String)
    | HydrophobicFitness (Maybe Order) (Maybe String)
    | MeanPackingDensity (Maybe Order) (Maybe String)
    | SequenceContains (Maybe String)
    | CompositionDeviation (Maybe UnitType) (Maybe String)
    | BUDEFFTotal (Maybe Order) (Maybe String)
    | EvoEF2Total (Maybe Order) (Maybe String)
    | DFIRE2Total (Maybe Order) (Maybe String)
    | RosettaTotal (Maybe Order) (Maybe String)
    | Agg3DTotal (Maybe Order) (Maybe String)


stringFromNewRequirement : NewRequirement NewRequirementData -> String
stringFromNewRequirement newRequirement =
    case newRequirement of
        Data data ->
            case data of
                Constant _ ->
                    "Constant"

                Value _ ->
                    "Value"

        Not _ ->
            "Not"

        Or _ _ ->
            "Or"

        And _ _ ->
            "And"


newRequirementToRequirement :
    NewRequirement NewRequirementData
    -> Result String (Requirement RequirementData)
newRequirementToRequirement newRequirement =
    case newRequirement of
        Data data ->
            case data of
                Constant mConstantType ->
                    let
                        constantConstructor =
                            Requirement.Constant
                                >> Requirement.Data
                    in
                    case mConstantType of
                        Nothing ->
                            Err "Constant was not fully defined."

                        Just constantType ->
                            case constantType of
                                Method Nothing ->
                                    Err "Constant was not fully defined."

                                Method (Just methodType) ->
                                    let
                                        methodConstructor =
                                            constantConstructor << Requirement.Method
                                    in
                                    (case methodType of
                                        SPPS ->
                                            methodConstructor Requirement.SPPS

                                        MolecularBiology ->
                                            methodConstructor Requirement.MolecularBiology
                                    )
                                        |> Ok

                Value mValueType ->
                    let
                        valueConstructor =
                            Requirement.Value
                                >> Requirement.Data
                    in
                    case mValueType of
                        Nothing ->
                            Err "Value was not fully defined."

                        Just valueType ->
                            case valueType of
                                IsoelectricPoint (Just order) (Just value) ->
                                    case String.toFloat value of
                                        Just floatValue ->
                                            Requirement.IsoelectricPoint order floatValue
                                                |> valueConstructor
                                                |> Ok

                                        Nothing ->
                                            Err <|
                                                "The value you've provided for isoelectric "
                                                    ++ "point is not a number."

                                IsoelectricPoint _ _ ->
                                    Err "Isoelectric point value was not fully defined."

                                HydrophobicFitness (Just order) (Just value) ->
                                    case String.toFloat value of
                                        Just floatValue ->
                                            Requirement.HydrophobicFitness order floatValue
                                                |> valueConstructor
                                                |> Ok

                                        Nothing ->
                                            Err <|
                                                "The value you've provided for hydrophobic "
                                                    ++ "fitness is not a number."

                                HydrophobicFitness _ _ ->
                                    Err "Hydrophobic fitness value was not fully defined."

                                MeanPackingDensity (Just order) (Just value) ->
                                    case String.toFloat value of
                                        Just floatValue ->
                                            Requirement.MeanPackingDensity order floatValue
                                                |> valueConstructor
                                                |> Ok

                                        Nothing ->
                                            Err <|
                                                "The value you've provided for mean "
                                                    ++ "packing density is not a number."

                                MeanPackingDensity _ _ ->
                                    Err "Mean packing density value was not fully defined."

                                SequenceContains (Just string) ->
                                    Requirement.SequenceContains string
                                        |> valueConstructor
                                        |> Ok

                                SequenceContains _ ->
                                    Err "Sequence contains value was not fully defined."

                                CompositionDeviation (Just unitType) (Just value) ->
                                    case String.toFloat value of
                                        Just floatValue ->
                                            Requirement.CompositionDeviation unitType floatValue
                                                |> valueConstructor
                                                |> Ok

                                        Nothing ->
                                            Err <|
                                                "The value you've provided for "
                                                    ++ "composition deviation is not a number."

                                CompositionDeviation _ _ ->
                                    Err "Composition deviation value was not fully defined."

                                BUDEFFTotal (Just order) (Just value) ->
                                    case String.toFloat value of
                                        Just floatValue ->
                                            Requirement.BUDEFFTotal order floatValue
                                                |> valueConstructor
                                                |> Ok

                                        Nothing ->
                                            Err <|
                                                "The value you've provided for BUDEFF Total Energy"
                                                    ++ "is not a number."

                                BUDEFFTotal _ _ ->
                                    Err "BUDEFF Total Energy value was not fully defined."

                                EvoEF2Total (Just order) (Just value) ->
                                    case String.toFloat value of
                                        Just floatValue ->
                                            Requirement.EvoEF2Total order floatValue
                                                |> valueConstructor
                                                |> Ok

                                        Nothing ->
                                            Err <|
                                                "The value you've provided for EvoEF2 Total Energy"
                                                    ++ "is not a number."

                                EvoEF2Total _ _ ->
                                    Err "EvoEF2 Total Energy value was not fully defined."

                                DFIRE2Total (Just order) (Just value) ->
                                    case String.toFloat value of
                                        Just floatValue ->
                                            Requirement.DFIRE2Total order floatValue
                                                |> valueConstructor
                                                |> Ok

                                        Nothing ->
                                            Err <|
                                                "The value you've provided for DFIRE2 Total Energy"
                                                    ++ "is not a number."

                                DFIRE2Total _ _ ->
                                    Err "DFIRE2 Total Energy value was not fully defined."

                                RosettaTotal (Just order) (Just value) ->
                                    case String.toFloat value of
                                        Just floatValue ->
                                            Requirement.RosettaTotal order floatValue
                                                |> valueConstructor
                                                |> Ok

                                        Nothing ->
                                            Err <|
                                                "The value you've provided for Rosetta Total Energy"
                                                    ++ "is not a number."

                                RosettaTotal _ _ ->
                                    Err "Rosetta Total Energy value was not fully defined."

                                Agg3DTotal (Just order) (Just value) ->
                                    case String.toFloat value of
                                        Just floatValue ->
                                            Requirement.Agg3DTotal order floatValue
                                                |> valueConstructor
                                                |> Ok

                                        Nothing ->
                                            Err <|
                                                "The value you've provided for Aggrescan3D Total Score"
                                                    ++ "is not a number."

                                Agg3DTotal _ _ ->
                                    Err "Aggrescan3D Total Score value was not fully defined."

        Not mNewSubRequirement ->
            case mNewSubRequirement of
                Nothing ->
                    Err "Not statement is not fully defined."

                Just newSubRequirement ->
                    newRequirementToRequirement newSubRequirement
                        |> Result.map Requirement.Not

        Or mNewSubRequirement1 mNewSubRequirement2 ->
            case ( mNewSubRequirement1, mNewSubRequirement2 ) of
                ( Just newSubRequirement1, Just newSubRequirement2 ) ->
                    Result.map2 Requirement.Or
                        (newRequirementToRequirement newSubRequirement1)
                        (newRequirementToRequirement newSubRequirement2)

                _ ->
                    Err "Or statement is not fully defined."

        And mNewSubRequirement1 mNewSubRequirement2 ->
            case ( mNewSubRequirement1, mNewSubRequirement2 ) of
                ( Just newSubRequirement1, Just newSubRequirement2 ) ->
                    Result.map2 Requirement.And
                        (newRequirementToRequirement newSubRequirement1)
                        (newRequirementToRequirement newSubRequirement2)

                _ ->
                    Err "And statement is not fully defined."



-- }}}
-- }}}
-- {{{ INIT


type alias Params =
    ()


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared { key } =
    let
        newModel =
            defaultModel key
    in
    case Shared.getRunState shared of
        Just runState ->
            ( { newModel
                | mResourceUuid = Just runState.resourceUuid
                , specifications = runState.specifications
              }
            , Cmd.none
            )

        Nothing ->
            ( newModel, Cmd.none )



-- }}}
-- {{{ UPDATE


type Msg
    = UpdatedName String
    | UpdatedDescription String
    | ClickedNewRequirement
    | CancelledAdd
    | UpdatedNewRequirement (Maybe (NewRequirement NewRequirementData))
    | ClickedAddRequirement
    | ClickedCreateSpecification
    | ClearPageErrors


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdatedName name ->
            if String.isEmpty name then
                ( { model | name = Nothing }
                , Cmd.none
                )

            else
                ( { model | name = Just name }
                , Cmd.none
                )

        UpdatedDescription description ->
            if String.isEmpty description then
                ( { model | description = Nothing }
                , Cmd.none
                )

            else
                ( { model | description = Just description }
                , Cmd.none
                )

        ClickedNewRequirement ->
            ( { model | mode = Add Nothing }
            , Cmd.none
            )

        CancelledAdd ->
            ( { model | mode = View }
            , Cmd.none
            )

        UpdatedNewRequirement requirement ->
            ( { model | mode = Add <| requirement }
            , Cmd.none
            )

        ClickedAddRequirement ->
            case model.mode of
                (Add (Just newRequirement)) as addMode ->
                    case newRequirementToRequirement newRequirement of
                        Ok requirement ->
                            ( { model
                                | mode = View
                                , requirements =
                                    requirement
                                        :: model.requirements
                              }
                            , Cmd.none
                            )

                        Err errorString ->
                            Error.updateWithError
                                ClearPageErrors
                                { model | mode = addMode }
                                { title = "Failed to add requirement"
                                , details = errorString
                                , severity = Error.Low
                                }

                Add Nothing ->
                    Error.updateWithError
                        ClearPageErrors
                        model
                        { title = "failed to add requirement"
                        , details =
                            """The add button should not have been visible as a new
                            requirement was not defined.
                            """
                        , severity = Error.Low
                        }

                View ->
                    Error.updateWithError
                        ClearPageErrors
                        { model | mode = View }
                        { title = "failed to add requirement"
                        , details =
                            """The add button should not have been visible as the app
                            was in view mode.
                            """
                        , severity = Error.Low
                        }

        ClickedCreateSpecification ->
            case model.mResourceUuid of
                Just resourceUuid ->
                    let
                        { uuidString, nextResourceUuid } =
                            ResourceUuid.toString
                                resourceUuid

                        specification =
                            { name =
                                Maybe.withDefault
                                    "DEFAULT SPEC NAME"
                                    model.name
                            , description =
                                Maybe.withDefault
                                    "DEFAULT SPEC DESCRIPTION"
                                    model.description
                            , requirements = Requirement.All model.requirements
                            , deleteStatus = Buttons.initDangerStatus
                            }

                        newModel =
                            defaultModel model.navKey
                    in
                    ( { newModel
                        | mResourceUuid = Just nextResourceUuid
                        , specifications =
                            Dict.insert uuidString
                                (Specification.createSpecificationStub specification
                                    |> Specification.storeSpecificationStubLocally
                                )
                                model.specifications
                      }
                    , Cmd.batch
                        [ Specification.storeSpecification
                            { uuidString = uuidString
                            , specification =
                                specification
                                    |> Codec.encoder
                                        Specification.codec
                            }
                        , navigate newModel.navKey Route.Specifications
                        ]
                    )

                Nothing ->
                    Error.updateWithError
                        ClearPageErrors
                        model
                        { title = "Failed to create specification"
                        , details =
                            """Failed to create a new specification set. Try refreshing
                            your browser to fix this, or if it persists, report it as a
                            bug.  See the home page for details on how to do this.
                            """
                        , severity = Error.Medium
                        }

        ClearPageErrors ->
            ( { model | pageErrors = [] }
            , Cmd.none
            )


save : Model -> Shared.Model -> Shared.Model
save model shared =
    let
        updatedShared =
            Error.updateSharedModelErrors model shared
    in
    case model.mResourceUuid of
        Just resourceUuid ->
            Shared.mapRunState
                (\runState ->
                    { runState
                        | resourceUuid = resourceUuid
                        , specifications = model.specifications
                        , saveStateRequested = True
                    }
                )
                updatedShared

        Nothing ->
            updatedShared


load : Shared.Model -> Model -> ( Model, Cmd Msg )
load shared model =
    case Shared.getRunState shared of
        Just runState ->
            ( { model
                | mResourceUuid = Just runState.resourceUuid
                , specifications = runState.specifications
              }
            , Cmd.none
            )

        Nothing ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- }}}
-- {{{ VIEW


view : Model -> Document Msg
view model =
    { title = "New Specification"
    , body = [ bodyView model ]
    }


bodyView : Model -> Element Msg
bodyView model =
    el [ centerX, width <| maximum 800 <| fill ] <|
        column
            [ width fill, spacing 15 ]
            [ text "Create New Specification"
                |> Style.h1
                |> el [ centerX ]
            , Input.text
                Style.textInputStyle
                { onChange = UpdatedName
                , text = Maybe.withDefault "" model.name
                , placeholder = Nothing
                , label =
                    Input.labelAbove []
                        (Style.h2 <| text "Name")
                }
            , Input.multiline
                Style.textInputStyle
                { onChange = UpdatedDescription
                , text = Maybe.withDefault "" model.description
                , placeholder = Nothing
                , label =
                    Input.labelAbove []
                        (Style.h2 <| text "Description")
                , spellcheck = True
                }
            , Style.h2 <| text "Requirements"
            , case model.mode of
                View ->
                    Buttons.alwaysActiveButton
                        { clickMsg = ClickedNewRequirement
                        , label = text "New Requirement"
                        , pressed = False
                        }

                Add mNewRequirement ->
                    addRequirementView mNewRequirement
            , el []
                (case model.requirements of
                    [] ->
                        paragraph []
                            [ text """No requirements defined. Click "New Requirement" to
                        get started."""
                            ]

                    _ ->
                        Requirement.requirementView <| Requirement.All model.requirements
                )
            , let
                areRequirements =
                    List.isEmpty model.requirements
                        |> not

                validName =
                    case model.name of
                        Nothing ->
                            False

                        Just _ ->
                            True

                validDescription =
                    case model.description of
                        Nothing ->
                            False

                        Just _ ->
                            True

                complete =
                    areRequirements && validName && validDescription
              in
              Buttons.conditionalButton
                { clickMsg =
                    Just ClickedCreateSpecification
                , label = text "Create Specification"
                , isActive = complete
                }
            ]


addRequirementView :
    Maybe (NewRequirement NewRequirementData)
    -> Element Msg
addRequirementView mNewRequirement =
    let
        ( requirementComplete, requirementView ) =
            newRequirementView UpdatedNewRequirement mNewRequirement
    in
    column
        (Style.defaultBorder
            ++ [ padding 5
               , spacing 10
               , Background.color Style.colorPalette.c5
               , Border.color Style.colorPalette.c1
               ]
        )
        [ requirementView
        , row
            [ spacing 10 ]
            [ Buttons.conditionalButton
                { clickMsg = Just ClickedAddRequirement
                , label = text "Add"
                , isActive = requirementComplete
                }
            , Buttons.alwaysActiveButton
                { clickMsg = CancelledAdd
                , label = text "Cancel"
                , pressed = False
                }
            ]
        ]


labelStyle : List (Attribute msg)
labelStyle =
    [ centerY
    , paddingXY 0 10
    , pointer
    ]


newRequirementView :
    (Maybe (NewRequirement NewRequirementData) -> Msg)
    -> Maybe (NewRequirement NewRequirementData)
    -> ( Bool, Element Msg )
newRequirementView msgConstructor mNewRequirement =
    case mNewRequirement of
        Nothing ->
            ( False
            , optionsView
                { msgConstructor = Just >> msgConstructor
                , optionName = "Requirement"
                , optionToString = stringFromNewRequirement
                , options =
                    [ Data <| Constant Nothing
                    , Data <| Value Nothing
                    , Not Nothing
                    , Or Nothing Nothing
                    , And Nothing Nothing
                    ]
                }
            )

        Just newRequirement ->
            let
                requirementLabel =
                    paragraph
                        ((Nothing
                            |> msgConstructor
                            |> Events.onClick
                         )
                            :: labelStyle
                        )
                        [ newRequirement
                            |> stringFromNewRequirement
                            |> text
                        ]
            in
            case newRequirement of
                Data data ->
                    case data of
                        Constant Nothing ->
                            ( False
                            , wrappedRow [ spacing 10 ]
                                [ requirementLabel
                                , optionsView
                                    { msgConstructor =
                                        \constantType ->
                                            Constant (Just constantType)
                                                |> Data
                                                |> Just
                                                |> msgConstructor
                                    , optionToString = stringFromConstantType
                                    , optionName = "Value"
                                    , options = [ Method Nothing ]
                                    }
                                ]
                            )

                        Constant (Just constantType) ->
                            constantTypeView
                                msgConstructor
                                constantType

                        Value Nothing ->
                            ( False
                            , wrappedRow [ spacing 10 ]
                                [ requirementLabel
                                , optionsView
                                    { msgConstructor =
                                        \valueType ->
                                            Value (Just valueType)
                                                |> Data
                                                |> Just
                                                |> msgConstructor
                                    , optionToString = stringFromValueType
                                    , optionName = "Type"
                                    , options =
                                        [ IsoelectricPoint Nothing Nothing
                                        , HydrophobicFitness Nothing Nothing
                                        , MeanPackingDensity Nothing Nothing
                                        , SequenceContains Nothing
                                        , CompositionDeviation Nothing Nothing
                                        , BUDEFFTotal Nothing Nothing
                                        , EvoEF2Total Nothing Nothing
                                        , DFIRE2Total Nothing Nothing
                                        , RosettaTotal Nothing Nothing
                                        , Agg3DTotal Nothing Nothing
                                        ]
                                    }
                                ]
                            )

                        Value (Just valueType) ->
                            valueTypeView
                                msgConstructor
                                valueType

                Not mNewSubRequirement ->
                    let
                        ( requirementComplete, requirementView ) =
                            newRequirementView
                                (Not >> Just >> msgConstructor)
                                mNewSubRequirement
                    in
                    ( requirementComplete
                    , column [ spacing 10 ]
                        [ requirementLabel
                        , el (Style.defaultBorder ++ [ padding 10 ]) requirementView
                        ]
                    )

                Or mNewSubRequirement1 mNewSubRequirement2 ->
                    let
                        ( requirementComplete1, requirementView1 ) =
                            newRequirementView
                                ((\r -> Or r mNewSubRequirement2)
                                    >> Just
                                    >> msgConstructor
                                )
                                mNewSubRequirement1

                        ( requirementComplete2, requirementView2 ) =
                            newRequirementView
                                ((\r -> Or mNewSubRequirement1 r)
                                    >> Just
                                    >> msgConstructor
                                )
                                mNewSubRequirement2
                    in
                    ( requirementComplete1 && requirementComplete2
                    , column [ spacing 10 ]
                        [ requirementLabel
                        , el (Style.defaultBorder ++ [ padding 10 ]) requirementView1
                        , el (Style.defaultBorder ++ [ padding 10 ]) requirementView2
                        ]
                    )

                And mNewSubRequirement1 mNewSubRequirement2 ->
                    let
                        ( requirementComplete1, requirementView1 ) =
                            newRequirementView
                                ((\r -> And r mNewSubRequirement2)
                                    >> Just
                                    >> msgConstructor
                                )
                                mNewSubRequirement1

                        ( requirementComplete2, requirementView2 ) =
                            newRequirementView
                                ((\r -> And mNewSubRequirement1 r)
                                    >> Just
                                    >> msgConstructor
                                )
                                mNewSubRequirement2
                    in
                    ( requirementComplete1 && requirementComplete2
                    , column [ spacing 10 ]
                        [ requirementLabel
                        , el (Style.defaultBorder ++ [ padding 10 ]) requirementView1
                        , el (Style.defaultBorder ++ [ padding 10 ]) requirementView2
                        ]
                    )


constantTypeView :
    (Maybe (NewRequirement NewRequirementData) -> Msg)
    -> ConstantType
    -> ( Bool, Element Msg )
constantTypeView msgConstructor constantType =
    let
        constantLabel =
            paragraph
                ((Nothing |> msgConstructor |> Events.onClick)
                    :: labelStyle
                )
                [ text "Constant" ]

        constantTypeLabel =
            paragraph
                ((Constant Nothing
                    |> Data
                    |> Just
                    |> msgConstructor
                    |> Events.onClick
                 )
                    :: labelStyle
                )
                [ constantType |> stringFromConstantType |> text ]

        constantTypeConstructor : ConstantType -> Msg
        constantTypeConstructor =
            Just
                >> Constant
                >> Data
                >> Just
                >> msgConstructor
    in
    case constantType of
        Method Nothing ->
            ( False
            , wrappedRow [ spacing 10 ]
                [ constantLabel
                , constantTypeLabel
                , optionsView
                    { msgConstructor =
                        \methodType ->
                            Method (Just methodType)
                                |> constantTypeConstructor
                    , optionToString = stringFromMethodType
                    , optionName = stringFromConstantType constantType
                    , options = [ SPPS, MolecularBiology ]
                    }
                ]
            )

        Method (Just methodType) ->
            ( True
            , wrappedRow [ spacing 10 ]
                [ constantLabel
                , constantTypeLabel
                , paragraph
                    [ Method Nothing
                        |> constantTypeConstructor
                        |> Events.onClick
                    ]
                    [ methodType |> stringFromMethodType |> text
                    ]
                ]
            )


valueTypeView :
    (Maybe (NewRequirement NewRequirementData) -> Msg)
    -> ValueType
    -> ( Bool, Element Msg )
valueTypeView msgConstructor valueType =
    let
        valueConstructor =
            Just
                >> Value
                >> Data
                >> Just
                >> msgConstructor

        valueLabel =
            paragraph
                ((Nothing
                    |> msgConstructor
                    |> Events.onClick
                 )
                    :: labelStyle
                )
                [ text "Value" ]

        valueTypeLabel =
            paragraph
                ((Value Nothing
                    |> Data
                    |> Just
                    |> msgConstructor
                    |> Events.onClick
                 )
                    :: labelStyle
                )
                [ stringFromValueType valueType
                    |> text
                ]

        orderLabel : Order -> Msg -> Element Msg
        orderLabel order noOrderMsg =
            paragraph
                ((noOrderMsg |> Events.onClick)
                    :: labelStyle
                )
                [ Requirement.stringFromOrder order
                    |> text
                ]

        unitTypeLabel : UnitType -> Msg -> Element Msg
        unitTypeLabel unitType noUnitTypeMsg =
            paragraph
                ((noUnitTypeMsg |> Events.onClick)
                    :: labelStyle
                )
                [ Requirement.stringFromUnitType unitType
                    |> text
                ]
    in
    case valueType of
        IsoelectricPoint Nothing Nothing ->
            valueOrderView
                { msgConstructor =
                    \order ->
                        IsoelectricPoint (Just order) Nothing
                            |> valueConstructor
                , valueLabel = valueLabel
                , valueTypeLabel = valueTypeLabel
                }

        IsoelectricPoint Nothing (Just _) ->
            ( False
            , paragraph []
                [ text <|
                    "Something went wrong while defining your value, hit "
                        ++ "cancel and start again."
                ]
            )

        IsoelectricPoint (Just order) mValue ->
            valueFloatInputView
                { mValue = mValue
                , msgConstructor =
                    (\s ->
                        Just s
                            |> IsoelectricPoint (Just order)
                    )
                        >> valueConstructor
                , labels =
                    [ valueLabel
                    , valueTypeLabel
                    , orderLabel order
                        (IsoelectricPoint Nothing Nothing
                            |> valueConstructor
                        )
                    ]
                }

        HydrophobicFitness Nothing Nothing ->
            valueOrderView
                { msgConstructor =
                    \order ->
                        HydrophobicFitness (Just order) Nothing
                            |> valueConstructor
                , valueLabel = valueLabel
                , valueTypeLabel = valueTypeLabel
                }

        HydrophobicFitness Nothing (Just _) ->
            ( False
            , paragraph []
                [ text <|
                    "Something went wrong while defining your value, hit "
                        ++ "cancel and start again."
                ]
            )

        HydrophobicFitness (Just order) mValue ->
            valueFloatInputView
                { mValue = mValue
                , labels =
                    [ valueLabel
                    , valueTypeLabel
                    , orderLabel
                        order
                        (HydrophobicFitness Nothing Nothing
                            |> valueConstructor
                        )
                    ]
                , msgConstructor =
                    \s ->
                        Just s
                            |> HydrophobicFitness (Just order)
                            |> valueConstructor
                }

        MeanPackingDensity Nothing Nothing ->
            valueOrderView
                { msgConstructor =
                    \order ->
                        MeanPackingDensity (Just order) Nothing
                            |> valueConstructor
                , valueLabel = valueLabel
                , valueTypeLabel = valueTypeLabel
                }

        MeanPackingDensity Nothing (Just _) ->
            ( False
            , paragraph []
                [ text <|
                    "Something went wrong while defining your value, hit "
                        ++ "cancel and start again."
                ]
            )

        MeanPackingDensity (Just order) mValue ->
            valueFloatInputView
                { mValue = mValue
                , labels =
                    [ valueLabel
                    , valueTypeLabel
                    , orderLabel
                        order
                        (MeanPackingDensity Nothing Nothing
                            |> valueConstructor
                        )
                    ]
                , msgConstructor =
                    \s ->
                        Just s
                            |> MeanPackingDensity (Just order)
                            |> valueConstructor
                }

        SequenceContains mValue ->
            valueSequenceInputView
                { valueLabel = valueLabel
                , valueTypeLabel = valueTypeLabel
                , mValue = mValue
                , msgConstructor =
                    \sequence ->
                        SequenceContains (Just sequence)
                            |> valueConstructor
                }

        CompositionDeviation Nothing Nothing ->
            valueUnitTypeView
                { msgConstructor =
                    \unitType ->
                        CompositionDeviation
                            (Just unitType)
                            Nothing
                            |> valueConstructor
                , labels =
                    [ valueLabel
                    , valueTypeLabel
                    ]
                }

        CompositionDeviation (Just unitType) mValue ->
            valueFloatInputView
                { mValue = mValue
                , labels =
                    [ valueLabel
                    , valueTypeLabel
                    , unitTypeLabel
                        unitType
                        (CompositionDeviation Nothing Nothing
                            |> valueConstructor
                        )
                    ]
                , msgConstructor =
                    \s ->
                        Just s
                            |> CompositionDeviation (Just unitType)
                            |> valueConstructor
                }

        CompositionDeviation _ _ ->
            ( False
            , paragraph []
                [ text <|
                    "Something went wrong while defining your value, hit "
                        ++ "cancel and start again."
                ]
            )

        BUDEFFTotal Nothing Nothing ->
            valueOrderView
                { msgConstructor =
                    \order ->
                        BUDEFFTotal (Just order) Nothing
                            |> valueConstructor
                , valueLabel = valueLabel
                , valueTypeLabel = valueTypeLabel
                }

        BUDEFFTotal Nothing (Just _) ->
            ( False
            , paragraph []
                [ text <|
                    "Something went wrong while defining your value, hit "
                        ++ "cancel and start again."
                ]
            )

        BUDEFFTotal (Just order) mValue ->
            valueFloatInputView
                { mValue = mValue
                , labels =
                    [ valueLabel
                    , valueTypeLabel
                    , orderLabel
                        order
                        (BUDEFFTotal Nothing Nothing
                            |> valueConstructor
                        )
                    ]
                , msgConstructor =
                    \s ->
                        Just s
                            |> BUDEFFTotal (Just order)
                            |> valueConstructor
                }

        EvoEF2Total Nothing Nothing ->
            valueOrderView
                { msgConstructor =
                    \order ->
                        EvoEF2Total (Just order) Nothing
                            |> valueConstructor
                , valueLabel = valueLabel
                , valueTypeLabel = valueTypeLabel
                }

        EvoEF2Total Nothing (Just _) ->
            ( False
            , paragraph []
                [ text <|
                    "Something went wrong while defining your value, hit "
                        ++ "cancel and start again."
                ]
            )

        EvoEF2Total (Just order) mValue ->
            valueFloatInputView
                { mValue = mValue
                , labels =
                    [ valueLabel
                    , valueTypeLabel
                    , orderLabel
                        order
                        (EvoEF2Total Nothing Nothing
                            |> valueConstructor
                        )
                    ]
                , msgConstructor =
                    \s ->
                        Just s
                            |> EvoEF2Total (Just order)
                            |> valueConstructor
                }

        DFIRE2Total Nothing Nothing ->
            valueOrderView
                { msgConstructor =
                    \order ->
                        DFIRE2Total (Just order) Nothing
                            |> valueConstructor
                , valueLabel = valueLabel
                , valueTypeLabel = valueTypeLabel
                }

        DFIRE2Total Nothing (Just _) ->
            ( False
            , paragraph []
                [ text <|
                    "Something went wrong while defining your value, hit "
                        ++ "cancel and start again."
                ]
            )

        DFIRE2Total (Just order) mValue ->
            valueFloatInputView
                { mValue = mValue
                , labels =
                    [ valueLabel
                    , valueTypeLabel
                    , orderLabel
                        order
                        (DFIRE2Total Nothing Nothing
                            |> valueConstructor
                        )
                    ]
                , msgConstructor =
                    \s ->
                        Just s
                            |> DFIRE2Total (Just order)
                            |> valueConstructor
                }

        RosettaTotal Nothing Nothing ->
            valueOrderView
                { msgConstructor =
                    \order ->
                        RosettaTotal (Just order) Nothing
                            |> valueConstructor
                , valueLabel = valueLabel
                , valueTypeLabel = valueTypeLabel
                }

        RosettaTotal Nothing (Just _) ->
            ( False
            , paragraph []
                [ text <|
                    "Something went wrong while defining your value, hit "
                        ++ "cancel and start again."
                ]
            )

        RosettaTotal (Just order) mValue ->
            valueFloatInputView
                { mValue = mValue
                , labels =
                    [ valueLabel
                    , valueTypeLabel
                    , orderLabel
                        order
                        (RosettaTotal Nothing Nothing
                            |> valueConstructor
                        )
                    ]
                , msgConstructor =
                    \s ->
                        Just s
                            |> RosettaTotal (Just order)
                            |> valueConstructor
                }

        Agg3DTotal Nothing Nothing ->
            valueOrderView
                { msgConstructor =
                    \order ->
                        Agg3DTotal (Just order) Nothing
                            |> valueConstructor
                , valueLabel = valueLabel
                , valueTypeLabel = valueTypeLabel
                }

        Agg3DTotal Nothing (Just _) ->
            ( False
            , paragraph []
                [ text <|
                    "Something went wrong while defining your value, hit "
                        ++ "cancel and start again."
                ]
            )

        Agg3DTotal (Just order) mValue ->
            valueFloatInputView
                { mValue = mValue
                , labels =
                    [ valueLabel
                    , valueTypeLabel
                    , orderLabel
                        order
                        (Agg3DTotal Nothing Nothing
                            |> valueConstructor
                        )
                    ]
                , msgConstructor =
                    \s ->
                        Just s
                            |> Agg3DTotal (Just order)
                            |> valueConstructor
                }


optionsView :
    { msgConstructor : a -> msg
    , optionToString : a -> String
    , optionName : String
    , options : List a
    }
    -> Element msg
optionsView { msgConstructor, optionToString, optionName, options } =
    let
        optionView option =
            paragraph
                (Style.defaultBorder
                    ++ [ padding 5
                       , pointer
                       , Events.onClick <| msgConstructor option
                       ]
                )
                [ optionToString option |> text ]
    in
    column
        [ spacing 10 ]
        (el [ Font.bold ] (text optionName)
            :: List.map optionView options
        )


valueOrderView :
    { msgConstructor : Order -> Msg
    , valueLabel : Element Msg
    , valueTypeLabel : Element Msg
    }
    -> ( Bool, Element Msg )
valueOrderView { valueTypeLabel, valueLabel, msgConstructor } =
    ( False
    , wrappedRow [ spacing 10 ]
        [ valueLabel
        , valueTypeLabel
        , optionsView
            { msgConstructor = msgConstructor
            , optionToString = Requirement.stringFromOrder
            , optionName = "Ordering"
            , options = [ LT, EQ, GT ]
            }
        ]
    )


valueUnitTypeView :
    { msgConstructor : UnitType -> Msg
    , labels : List (Element Msg)
    }
    -> ( Bool, Element Msg )
valueUnitTypeView { msgConstructor, labels } =
    ( False
    , wrappedRow [ spacing 10 ]
        (labels
            ++ [ optionsView
                    { msgConstructor = msgConstructor
                    , optionToString = Requirement.stringFromUnitType
                    , optionName = "Unit Type"
                    , options = [ StdDevs, Percent ]
                    }
               ]
        )
    )



-- valueRelationshipFloatInputView :
--     { msgConstructor : Msg
--     , inputTextMsg : String -> Msg
--     , maybeString : Maybe String
--     , labels : List (Element Msg)
--     }
--     -> ( Bool, Element Msg )
-- valueRelationshipFloatInputView { msgConstructor, inputTextMsg, maybeString, labels } =
--     let
--         ( floatString, completionStatus ) =
--             case maybeString of
--                 Just string ->
--                     ( string, True )
--                 Nothing ->
--                     ( "", False )
--     in
--     ( completionStatus
--     , row [ spacing 10 ]
--         (labels
--             ++ [ row [ spacing 5 ]
--                     [ Input.text
--                         Style.textInputStyle
--                         { onChange =
--                             inputTextMsg
--                         , text = floatString
--                         , placeholder = Nothing
--                         , label =
--                             Input.labelAbove []
--                                 (el [ Font.bold ] <| text "Value")
--                         }
--                     , el [ alignBottom ] <|
--                         Buttons.conditionalButton
--                             { label = text "Ok"
--                             , clickMsg = Just msgConstructor
--                             , isActive = completionStatus
--                             }
--                     ]
--                ]
--         )
--     )


valueFloatInputView :
    { mValue : Maybe String
    , msgConstructor : String -> Msg
    , labels : List (Element Msg)
    }
    -> ( Bool, Element Msg )
valueFloatInputView { mValue, msgConstructor, labels } =
    ( case mValue of
        Nothing ->
            False

        Just value ->
            case String.toFloat value of
                Just _ ->
                    True

                _ ->
                    False
    , wrappedRow [ spacing 10 ]
        (labels
            ++ [ Input.text
                    Style.textInputStyle
                    { onChange =
                        msgConstructor
                    , text = Maybe.withDefault "" mValue
                    , placeholder = Nothing
                    , label =
                        Input.labelAbove []
                            (el [ Font.bold ] <| text "Value")
                    }
               ]
        )
    )


valueSequenceInputView :
    { mValue : Maybe String
    , msgConstructor : String -> Msg
    , valueLabel : Element Msg
    , valueTypeLabel : Element Msg
    }
    -> ( Bool, Element Msg )
valueSequenceInputView { valueLabel, valueTypeLabel, msgConstructor, mValue } =
    ( case mValue of
        Nothing ->
            False

        Just _ ->
            True
    , wrappedRow [ spacing 10 ]
        [ valueLabel
        , valueTypeLabel
        , Input.text
            Style.textInputStyle
            { onChange =
                msgConstructor
            , text = Maybe.withDefault "" mValue
            , placeholder = Nothing
            , label =
                Input.labelAbove []
                    (el [ Font.bold ] <| text "Sequence")
            }
        ]
    )



-- }}}
