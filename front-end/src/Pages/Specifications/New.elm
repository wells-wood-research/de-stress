module Pages.Specifications.New exposing (Model, Msg, page)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Generated.Specifications.Params as Params
import Global
import Pages.Specifications.Dynamic exposing (requirementView)
import Spa.Page exposing (send)
import Specification as Specs
    exposing
        ( Requirement
        , RequirementData
        )
import Style
import Utils.Spa exposing (Page)


page : Page Params.New Model Msg model msg appMsg
page =
    Spa.Page.component
        { title = always "Specifications.New"
        , init = always init
        , update = always update
        , subscriptions = always subscriptions
        , view = always view
        }



-- {{{ Model


type alias Model =
    { requirements : List (Requirement RequirementData)
    , name : Maybe String
    , description : Maybe String
    , mode : Mode
    , errors : List String
    }


defaultModel : Model
defaultModel =
    { requirements = []
    , name = Nothing
    , description = Nothing
    , mode = View
    , errors = []
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


type ValueType
    = IsoelectricPoint (Maybe Order) (Maybe String)
    | HydrophobicFitness (Maybe Order) (Maybe String)
    | MeanPackingDensity (Maybe Order) (Maybe String)
    | SequenceContains (Maybe String)


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
                            Specs.Constant
                                >> Specs.Data
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
                                            constantConstructor << Specs.Method
                                    in
                                    (case methodType of
                                        SPPS ->
                                            methodConstructor Specs.SPPS

                                        MolecularBiology ->
                                            methodConstructor Specs.MolecularBiology
                                    )
                                        |> Ok

                Value mValueType ->
                    let
                        valueConstructor =
                            Specs.Value
                                >> Specs.Data
                    in
                    case mValueType of
                        Nothing ->
                            Err "Value was not fully defined."

                        Just valueType ->
                            case valueType of
                                IsoelectricPoint (Just order) (Just value) ->
                                    case String.toFloat value of
                                        Just floatValue ->
                                            Specs.IsoelectricPoint order floatValue
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
                                            Specs.HydrophobicFitness order floatValue
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
                                            Specs.MeanPackingDensity order floatValue
                                                |> valueConstructor
                                                |> Ok

                                        Nothing ->
                                            Err <|
                                                "The value you've provided for mean "
                                                    ++ "packing density is not a number."

                                MeanPackingDensity _ _ ->
                                    Err "Mean packing density value was not fully defined."

                                SequenceContains (Just string) ->
                                    Specs.SequenceContains string
                                        |> valueConstructor
                                        |> Ok

                                SequenceContains _ ->
                                    Err "Sequence contains value was not fully defined."

        Not mNewSubRequirement ->
            case mNewSubRequirement of
                Nothing ->
                    Err "Not statement is not fully defined."

                Just newSubRequirement ->
                    newRequirementToRequirement newSubRequirement
                        |> Result.map Specs.Not

        Or mNewSubRequirement1 mNewSubRequirement2 ->
            case ( mNewSubRequirement1, mNewSubRequirement2 ) of
                ( Just newSubRequirement1, Just newSubRequirement2 ) ->
                    Result.map2 Specs.Or
                        (newRequirementToRequirement newSubRequirement1)
                        (newRequirementToRequirement newSubRequirement2)

                _ ->
                    Err "Or statement is not fully defined."

        And mNewSubRequirement1 mNewSubRequirement2 ->
            case ( mNewSubRequirement1, mNewSubRequirement2 ) of
                ( Just newSubRequirement1, Just newSubRequirement2 ) ->
                    Result.map2 Specs.And
                        (newRequirementToRequirement newSubRequirement1)
                        (newRequirementToRequirement newSubRequirement2)

                _ ->
                    Err "And statement is not fully defined."



-- }}}
-- }}}
-- {{{ Init


init : Params.New -> ( Model, Cmd Msg, Cmd Global.Msg )
init _ =
    ( defaultModel
    , Cmd.none
    , Cmd.none
    )



-- }}}
-- {{{ Update


type Msg
    = UpdatedName String
    | UpdatedDescription String
    | ClickedNewRequirement
    | CancelledAdd
    | UpdatedNewRequirement (NewRequirement NewRequirementData)
    | ClickedAddRequirement
    | ClickedCreateSpecification


update : Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update msg model =
    case msg of
        UpdatedName name ->
            if String.isEmpty name then
                ( { model | name = Nothing }
                , Cmd.none
                , Cmd.none
                )

            else
                ( { model | name = Just name }
                , Cmd.none
                , Cmd.none
                )

        UpdatedDescription description ->
            if String.isEmpty description then
                ( { model | description = Nothing }
                , Cmd.none
                , Cmd.none
                )

            else
                ( { model | description = Just description }
                , Cmd.none
                , Cmd.none
                )

        ClickedNewRequirement ->
            ( { model | mode = Add Nothing }
            , Cmd.none
            , Cmd.none
            )

        CancelledAdd ->
            ( { model | mode = View }
            , Cmd.none
            , Cmd.none
            )

        UpdatedNewRequirement requirement ->
            ( { model | mode = Add <| Just requirement }
            , Cmd.none
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
                            , Cmd.none
                            )

                        Err errorString ->
                            ( { model
                                | mode = addMode
                                , errors = errorString :: model.errors
                              }
                            , Cmd.none
                            , Cmd.none
                            )

                Add Nothing ->
                    ( { model
                        | mode = View
                        , errors =
                            ("The add button should not have been visible as a new "
                                ++ "requirement was not defined."
                            )
                                :: model.errors
                      }
                    , Cmd.none
                    , Cmd.none
                    )

                View ->
                    ( { model
                        | mode = View
                        , errors =
                            ("The add button should not have been visible as the app "
                                ++ "was in view mode."
                            )
                                :: model.errors
                      }
                    , Cmd.none
                    , Cmd.none
                    )

        ClickedCreateSpecification ->
            ( defaultModel
            , Cmd.none
            , Global.AddSpecification
                { name =
                    Maybe.withDefault
                        "DEFAULT SPEC NAME"
                        model.name
                , description =
                    Maybe.withDefault
                        "DEFAULT SPEC DESCRIPTION"
                        model.description
                , requirements = Specs.All model.requirements
                , deleteStatus = Style.Unclicked
                }
                |> send
            )



-- }}}
-- {{{ Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- }}}
-- {{{ View


view : Model -> Element Msg
view model =
    column
        [ width fill, spacing 30 ]
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
                Style.alwaysActiveButton
                    { clickMsg = ClickedNewRequirement
                    , labelText = "New Requirement"
                    }

            Add mNewRequirement ->
                addRequirementView model.errors mNewRequirement
        , el []
            (case model.requirements of
                [] ->
                    paragraph []
                        [ text """No requirements defined. Click "New Requirement" to
                        get started."""
                        ]

                _ ->
                    requirementView <| Specs.All model.requirements
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
          Style.conditionalButton
            { clickMsg =
                ClickedCreateSpecification
            , labelText = "Create Specification"
            , isActive = complete
            }
        ]


addRequirementView :
    List String
    -> Maybe (NewRequirement NewRequirementData)
    -> Element Msg
addRequirementView errors mNewRequirement =
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
        [ text "New Requirement"
        , case errors of
            [] ->
                none

            _ ->
                column [ spacing 10 ]
                    ([ el [ Font.bold ] (text "Errors")
                     , text <|
                        "It looks like something went wrong while defining "
                            ++ "your specification."
                     ]
                        ++ List.map text errors
                    )
        , requirementView
        , row
            [ spacing 10 ]
            [ Style.conditionalButton
                { clickMsg = ClickedAddRequirement
                , labelText = "Add"
                , isActive = requirementComplete
                }
            , Style.alwaysActiveButton
                { clickMsg = CancelledAdd
                , labelText = "Cancel"
                }
            ]
        ]


newRequirementView :
    (NewRequirement NewRequirementData -> Msg)
    -> Maybe (NewRequirement NewRequirementData)
    -> ( Bool, Element Msg )
newRequirementView msgConstructor mNewRequirement =
    case mNewRequirement of
        Nothing ->
            ( False
            , optionsView
                { msgConstructor = msgConstructor
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
            case newRequirement of
                Data data ->
                    case data of
                        Constant Nothing ->
                            ( False
                            , row [ spacing 10 ]
                                [ (stringFromNewRequirement >> text) newRequirement
                                , optionsView
                                    { msgConstructor =
                                        \constantType ->
                                            Constant (Just constantType)
                                                |> Data
                                                |> msgConstructor
                                    , optionToString = stringFromConstantType
                                    , optionName = "Value"
                                    , options = [ Method Nothing ]
                                    }
                                ]
                            )

                        Constant (Just ((Method Nothing) as constantType)) ->
                            ( False
                            , row [ spacing 10 ]
                                [ (stringFromNewRequirement >> text) newRequirement
                                , (stringFromConstantType >> text) constantType
                                , optionsView
                                    { msgConstructor =
                                        \methodType ->
                                            Constant (Just (Method (Just methodType)))
                                                |> Data
                                                |> msgConstructor
                                    , optionToString = stringFromMethodType
                                    , optionName = stringFromConstantType constantType
                                    , options = [ SPPS, MolecularBiology ]
                                    }
                                ]
                            )

                        Constant (Just ((Method (Just methodType)) as constantType)) ->
                            ( True
                            , row [ spacing 10 ]
                                [ (stringFromNewRequirement >> text) newRequirement
                                , (stringFromConstantType >> text) constantType
                                , (stringFromMethodType >> text) methodType
                                ]
                            )

                        Value Nothing ->
                            ( False
                            , row [ spacing 10 ]
                                [ (stringFromNewRequirement >> text) newRequirement
                                , optionsView
                                    { msgConstructor =
                                        \valueType ->
                                            Value (Just valueType)
                                                |> Data
                                                |> msgConstructor
                                    , optionToString = stringFromValueType
                                    , optionName = "Type"
                                    , options =
                                        [ IsoelectricPoint Nothing Nothing
                                        , HydrophobicFitness Nothing Nothing
                                        , MeanPackingDensity Nothing Nothing
                                        , SequenceContains Nothing
                                        ]
                                    }
                                ]
                            )

                        Value (Just ((IsoelectricPoint Nothing Nothing) as valueType)) ->
                            valueOrderView
                                { valueTypeName = stringFromValueType valueType
                                , msgConstructor =
                                    \order ->
                                        Value (Just (IsoelectricPoint (Just order) Nothing))
                                            |> Data
                                            |> msgConstructor
                                }

                        Value (Just (IsoelectricPoint Nothing (Just _))) ->
                            ( False
                            , text <|
                                "Something went wrong while defining your value, hit "
                                    ++ "cancel and start again."
                            )

                        Value (Just ((IsoelectricPoint (Just order) mValue) as valueType)) ->
                            valueFloatInputView
                                { valueTypeName = stringFromValueType valueType
                                , mValue = mValue
                                , order = order
                                , msgConstructor =
                                    (\s ->
                                        Just s
                                            |> IsoelectricPoint (Just order)
                                            |> Just
                                            |> Value
                                            |> Data
                                    )
                                        >> msgConstructor
                                }

                        Value (Just ((HydrophobicFitness Nothing Nothing) as valueType)) ->
                            valueOrderView
                                { valueTypeName = stringFromValueType valueType
                                , msgConstructor =
                                    \order ->
                                        Value (Just (HydrophobicFitness (Just order) Nothing))
                                            |> Data
                                            |> msgConstructor
                                }

                        Value (Just (HydrophobicFitness Nothing (Just _))) ->
                            ( False
                            , text <|
                                "Something went wrong while defining your value, hit "
                                    ++ "cancel and start again."
                            )

                        Value (Just ((HydrophobicFitness (Just order) mValue) as valueType)) ->
                            valueFloatInputView
                                { valueTypeName = stringFromValueType valueType
                                , mValue = mValue
                                , order = order
                                , msgConstructor =
                                    (\s ->
                                        Just s
                                            |> HydrophobicFitness (Just order)
                                            |> Just
                                            |> Value
                                            |> Data
                                    )
                                        >> msgConstructor
                                }

                        Value (Just ((MeanPackingDensity Nothing Nothing) as valueType)) ->
                            valueOrderView
                                { valueTypeName = stringFromValueType valueType
                                , msgConstructor =
                                    \order ->
                                        Value (Just (MeanPackingDensity (Just order) Nothing))
                                            |> Data
                                            |> msgConstructor
                                }

                        Value (Just (MeanPackingDensity Nothing (Just _))) ->
                            ( False
                            , text <|
                                "Something went wrong while defining your value, hit "
                                    ++ "cancel and start again."
                            )

                        Value (Just ((MeanPackingDensity (Just order) mValue) as valueType)) ->
                            valueFloatInputView
                                { valueTypeName = stringFromValueType valueType
                                , mValue = mValue
                                , order = order
                                , msgConstructor =
                                    (\s ->
                                        Just s
                                            |> MeanPackingDensity (Just order)
                                            |> Just
                                            |> Value
                                            |> Data
                                    )
                                        >> msgConstructor
                                }

                        Value (Just ((SequenceContains mValue) as valueType)) ->
                            valueSequenceInputView
                                { valueTypeName = stringFromValueType valueType
                                , mValue = mValue
                                , msgConstructor =
                                    \sequence ->
                                        Value (Just (SequenceContains (Just sequence)))
                                            |> Data
                                            |> msgConstructor
                                }

                Not mNewSubRequirement ->
                    let
                        ( requirementComplete, requirementView ) =
                            newRequirementView
                                (Just >> Not >> msgConstructor)
                                mNewSubRequirement
                    in
                    ( requirementComplete
                    , column [ spacing 10 ]
                        [ el [ Font.bold ] (text "Not")
                        , el (Style.defaultBorder ++ [ padding 10 ]) requirementView
                        ]
                    )

                Or mNewSubRequirement1 mNewSubRequirement2 ->
                    let
                        ( requirementComplete1, requirementView1 ) =
                            newRequirementView
                                ((\r -> Or (Just r) mNewSubRequirement2)
                                    >> msgConstructor
                                )
                                mNewSubRequirement1

                        ( requirementComplete2, requirementView2 ) =
                            newRequirementView
                                ((\r -> Or mNewSubRequirement1 (Just r))
                                    >> msgConstructor
                                )
                                mNewSubRequirement2
                    in
                    ( requirementComplete1 && requirementComplete2
                    , column [ spacing 10 ]
                        [ el [ Font.bold ] (text "Or")
                        , el (Style.defaultBorder ++ [ padding 10 ]) requirementView1
                        , el (Style.defaultBorder ++ [ padding 10 ]) requirementView2
                        ]
                    )

                And mNewSubRequirement1 mNewSubRequirement2 ->
                    let
                        ( requirementComplete1, requirementView1 ) =
                            newRequirementView
                                ((\r -> And (Just r) mNewSubRequirement2)
                                    >> msgConstructor
                                )
                                mNewSubRequirement1

                        ( requirementComplete2, requirementView2 ) =
                            newRequirementView
                                ((\r -> And mNewSubRequirement1 (Just r))
                                    >> msgConstructor
                                )
                                mNewSubRequirement2
                    in
                    ( requirementComplete1 && requirementComplete2
                    , column [ spacing 10 ]
                        [ el [ Font.bold ] (text "And")
                        , el (Style.defaultBorder ++ [ padding 10 ]) requirementView1
                        , el (Style.defaultBorder ++ [ padding 10 ]) requirementView2
                        ]
                    )


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
            el [ Events.onClick <| msgConstructor option ]
                (optionToString option |> text)
    in
    column
        [ spacing 10 ]
        (el [ Font.bold ] (text optionName)
            :: List.map optionView options
        )


valueOrderView :
    { valueTypeName : String
    , msgConstructor : Order -> Msg
    }
    -> ( Bool, Element Msg )
valueOrderView { valueTypeName, msgConstructor } =
    ( False
    , row [ spacing 10 ]
        [ text "Value"
        , text valueTypeName
        , optionsView
            { msgConstructor = msgConstructor
            , optionToString = Specs.stringFromOrder
            , optionName = "Ordering"
            , options = [ LT, EQ, GT ]
            }
        ]
    )


valueFloatInputView :
    { valueTypeName : String
    , mValue : Maybe String
    , order : Order
    , msgConstructor : String -> Msg
    }
    -> ( Bool, Element Msg )
valueFloatInputView { msgConstructor, valueTypeName, mValue, order } =
    ( case mValue of
        Nothing ->
            False

        Just value ->
            case String.toFloat value of
                Just _ ->
                    True

                _ ->
                    False
    , row [ spacing 10 ]
        [ text "Value"
        , text valueTypeName
        , (Specs.stringFromOrder >> text) order
        , Input.text
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


valueSequenceInputView :
    { valueTypeName : String
    , mValue : Maybe String
    , msgConstructor : String -> Msg
    }
    -> ( Bool, Element Msg )
valueSequenceInputView { msgConstructor, valueTypeName, mValue } =
    ( case mValue of
        Nothing ->
            False

        Just _ ->
            True
    , row [ spacing 10 ]
        [ text "Value"
        , text valueTypeName
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
