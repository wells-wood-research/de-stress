module Pages.DeStress.ReferenceSets.New exposing (Model, Msg, Params, page)

import Browser.Navigation as Nav
import Codec
import Dict exposing (Dict)
import Element exposing (..)
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import RemoteData
import Set exposing (Set)
import Shared
import Shared.Buttons as Buttons
import Shared.Error as Error
import Shared.Metrics as Metrics
import Shared.ReferenceSet as ReferenceSet exposing (ReferenceSetRemoteData)
import Shared.ResourceUuid as ResourceUuid exposing (ResourceUuid)
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
    , newReferenceSet : NewReferenceSet
    , referenceSets : Dict String ReferenceSet.StoredReferenceSet
    , navKey : Nav.Key
    , pageErrors : List Error.Error
    }


type NewReferenceSet
    = NewHighResBiolUnit ReferenceSetRemoteData
    | NewPdbCodeList NewPdbCodeListParams


type alias NewPdbCodeListParams =
    { mName : Maybe String
    , mDescription : Maybe String
    , rawPdbCodeListInput : String
    , pdbCodeList : Set String
    , malformedPdbCodes : Set String
    , remoteData : ReferenceSetRemoteData
    }


defaultCodeListParams : NewPdbCodeListParams
defaultCodeListParams =
    { mName = Nothing
    , mDescription = Nothing
    , rawPdbCodeListInput = ""
    , pdbCodeList = Set.empty
    , malformedPdbCodes = Set.empty
    , remoteData = RemoteData.NotAsked
    }


stringFromModelType : NewReferenceSet -> String
stringFromModelType newReferenceSet =
    case newReferenceSet of
        NewHighResBiolUnit _ ->
            ReferenceSet.highResBiolUnits.name

        NewPdbCodeList _ ->
            "PDB Subset"


isDownloadingData : NewReferenceSet -> Bool
isDownloadingData newReferenceSet =
    let
        modelRemoteData =
            case newReferenceSet of
                NewHighResBiolUnit remoteData ->
                    remoteData

                NewPdbCodeList { remoteData } ->
                    remoteData
    in
    case modelRemoteData of
        RemoteData.Loading ->
            True

        _ ->
            False



-- }}}
-- {{{ INIT


type alias Params =
    ()


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared _ =
    case Shared.getRunState shared of
        Just runState ->
            ( { mResourceUuid = Just runState.resourceUuid
              , newReferenceSet = NewHighResBiolUnit RemoteData.NotAsked
              , referenceSets = runState.referenceSets
              , navKey = shared.key
              , pageErrors = []
              }
            , Cmd.none
            )

        Nothing ->
            ( { mResourceUuid = Nothing
              , newReferenceSet = NewHighResBiolUnit RemoteData.NotAsked
              , referenceSets = Dict.empty
              , navKey = shared.key
              , pageErrors = []
              }
            , Cmd.none
            )



-- }}}
-- {{{ UPDATE


type Msg
    = UpdatedType NewReferenceSet
    | ClickedDownloadDefaultHighRes
    | GotHighResBiolUnitsData ReferenceSetRemoteData
    | UpdatedName NewPdbCodeListParams String
    | UpdatedDescription NewPdbCodeListParams String
    | UpdatedPdbCodes NewPdbCodeListParams String
    | ClickedDownloadPreferredSubset
    | GotPreferredSubsetData ReferenceSetRemoteData
    | ClearPageErrors


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdatedType newReferenceSet ->
            ( { model | newReferenceSet = newReferenceSet }
            , Cmd.none
            )

        -- High Res Biol Unit
        ClickedDownloadDefaultHighRes ->
            ( { model | newReferenceSet = NewHighResBiolUnit RemoteData.Loading }
            , ReferenceSet.queryToCmd
                ReferenceSet.highResBiolUnits.query
                GotHighResBiolUnitsData
            )

        GotHighResBiolUnitsData remoteData ->
            case remoteData of
                RemoteData.Success metrics ->
                    let
                        referenceSet =
                            ReferenceSet.HighResBiolUnit
                                { metrics = metrics
                                , aggregateData =
                                    Metrics.createAggregateData
                                        metrics
                                , deleteStatus = Buttons.initDangerStatus
                                }

                        uuidString =
                            -- The name of the default reference set as the
                            -- id instead of a UUID.
                            ReferenceSet.highResBiolUnits.id
                    in
                    ( { model
                        | newReferenceSet = NewHighResBiolUnit remoteData
                        , referenceSets =
                            Dict.insert
                                uuidString
                                (referenceSet
                                    |> ReferenceSet.createReferenceSetStub
                                    |> ReferenceSet.storeReferenceSetStubLocally
                                )
                                model.referenceSets
                      }
                    , Cmd.batch
                        [ ReferenceSet.storeReferenceSet
                            { uuidString = uuidString
                            , referenceSet =
                                referenceSet
                                    |> Codec.encoder
                                        ReferenceSet.codec
                            }
                        , navigate model.navKey Route.DeStress__ReferenceSets
                        ]
                    )

                _ ->
                    ( { model | newReferenceSet = NewHighResBiolUnit remoteData }
                    , Cmd.none
                    )

        -- Preferred State Subset
        UpdatedName params name ->
            if String.isEmpty name then
                ( { model
                    | newReferenceSet =
                        { params | mName = Nothing }
                            |> NewPdbCodeList
                  }
                , Cmd.none
                )

            else
                ( { model
                    | newReferenceSet =
                        { params | mName = Just name }
                            |> NewPdbCodeList
                  }
                , Cmd.none
                )

        UpdatedDescription params description ->
            if String.isEmpty description then
                ( { model
                    | newReferenceSet =
                        { params | mDescription = Nothing }
                            |> NewPdbCodeList
                  }
                , Cmd.none
                )

            else
                ( { model
                    | newReferenceSet =
                        { params | mDescription = Just description }
                            |> NewPdbCodeList
                  }
                , Cmd.none
                )

        UpdatedPdbCodes params rawCodes ->
            let
                isPdbCode str =
                    (String.length str == 4)
                        && (String.toList str
                                |> List.all Char.isAlphaNum
                           )

                ( good, malformed ) =
                    String.words rawCodes
                        |> Set.fromList
                        |> Set.partition isPdbCode
            in
            ( { model
                | newReferenceSet =
                    { params
                        | rawPdbCodeListInput = rawCodes
                        , pdbCodeList = good
                        , malformedPdbCodes = malformed
                    }
                        |> NewPdbCodeList
              }
            , Cmd.none
            )

        ClickedDownloadPreferredSubset ->
            case model.newReferenceSet of
                NewPdbCodeList params ->
                    ( { model
                        | newReferenceSet =
                            NewPdbCodeList
                                { params
                                    | remoteData = RemoteData.Loading
                                }
                      }
                    , ReferenceSet.queryToCmd
                        (ReferenceSet.preferredStatesSubsetQuery params.pdbCodeList)
                        GotPreferredSubsetData
                    )

                NewHighResBiolUnit _ ->
                    Error.updateWithError
                        ClearPageErrors
                        model
                        { title = "Unexpected reference set type"
                        , details =
                            """Failed to create a new reference set. The type of
                                reference set you're creating shouldn't have allowed you
                                to click this button!  Try refreshing your browser to
                                fix this, or if it persists, report it as a bug.  See
                                the home page for details on how to do this.
                            """
                        , severity = Error.Medium
                        }

        GotPreferredSubsetData remoteData ->
            case ( model.newReferenceSet, model.mResourceUuid, remoteData ) of
                ( NewPdbCodeList params, Just resourceUuid, RemoteData.Success metrics ) ->
                    let
                        { uuidString, nextResourceUuid } =
                            ResourceUuid.toString
                                resourceUuid

                        referenceSet =
                            ReferenceSet.PdbCodeList
                                { metrics = metrics
                                , aggregateData = Metrics.createAggregateData metrics
                                , name =
                                    Maybe.withDefault "NAME"
                                        params.mName
                                , description =
                                    Maybe.withDefault "DESCRIPTION"
                                        params.mDescription
                                , pdbCodeList = params.pdbCodeList
                                , deleteStatus = Buttons.initDangerStatus
                                }
                    in
                    ( { model
                        | newReferenceSet = NewHighResBiolUnit RemoteData.NotAsked
                        , mResourceUuid = Just nextResourceUuid
                        , referenceSets =
                            Dict.insert
                                uuidString
                                (referenceSet
                                    |> ReferenceSet.createReferenceSetStub
                                    |> ReferenceSet.storeReferenceSetStubLocally
                                )
                                model.referenceSets
                      }
                    , Cmd.batch
                        [ ReferenceSet.storeReferenceSet
                            { uuidString = uuidString
                            , referenceSet =
                                Codec.encoder
                                    ReferenceSet.codec
                                    referenceSet
                            }
                        , navigate model.navKey Route.DeStress__ReferenceSets
                        ]
                    )

                _ ->
                    Error.updateWithError
                        ClearPageErrors
                        model
                        { title = "Failed to create reference set"
                        , details =
                            """Failed to create a new reference set. Try refreshing your
                            browser to fix this, or if it persists, report it as a bug.
                            See the home page for details on how to do this.
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
                        , referenceSets = model.referenceSets
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
                , referenceSets = runState.referenceSets
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
    { title = "Create New Reference Set"
    , body = [ bodyView model ]
    }


bodyView : Model -> Element Msg
bodyView model =
    column
        [ Style.pageWidths.singleColumn
        , centerX
        , spacing 30
        ]
    <|
        if isDownloadingData model.newReferenceSet then
            [ text "Downloading metric data..." ]

        else
            [ text "Create New Reference Set"
                |> Style.h1
                |> el [ centerX ]
            , row [ centerX, spacing 15 ] <|
                (List.map
                    (refSetTypeSelector model.newReferenceSet)
                    [ NewHighResBiolUnit RemoteData.NotAsked
                    , NewPdbCodeList defaultCodeListParams
                    ]
                    |> List.intersperse (text "|")
                )
            , case model.newReferenceSet of
                NewHighResBiolUnit _ ->
                    newHighResBiolUnitsView

                NewPdbCodeList params ->
                    newPdbCodeListView params
            ]


refSetTypeSelector : NewReferenceSet -> NewReferenceSet -> Element Msg
refSetTypeSelector current option =
    stringFromModelType option
        |> text
        |> el
            ((if stringFromModelType current == stringFromModelType option then
                [ Font.underline ]

              else
                [ Events.onClick <| UpdatedType option ]
             )
                ++ [ pointer ]
            )


newHighResBiolUnitsView : Element Msg
newHighResBiolUnitsView =
    column [ width fill, spacing 30 ]
        [ paragraph []
            [ text ReferenceSet.highResBiolUnits.description
            ]
        , Buttons.alwaysActiveButton
            { clickMsg =
                ClickedDownloadDefaultHighRes
            , label = text "Download Reference Set"
            , pressed = False
            }
        ]


newPdbCodeListView : NewPdbCodeListParams -> Element Msg
newPdbCodeListView params =
    column [ width fill, spacing 30 ]
        [ paragraph []
            [ text
                """Create a reference set from a list of PDB codes. The biological unit
                of the structure, as defined on PDBe as assembly 1, will be used to
                create the reference set.
                """
            ]
        , Input.text
            Style.textInputStyle
            { onChange = UpdatedName params
            , text = Maybe.withDefault "" params.mName
            , placeholder = Nothing
            , label =
                Input.labelAbove []
                    (Style.h2 <| text "Name")
            }
        , Input.multiline
            Style.textInputStyle
            { onChange = UpdatedDescription params
            , text = Maybe.withDefault "" params.mDescription
            , placeholder = Nothing
            , label =
                Input.labelAbove []
                    (Style.h2 <| text "Description")
            , spellcheck = False
            }
        , Input.multiline
            (Style.textInputStyle
                ++ [ height <| px 200 ]
            )
            { onChange = UpdatedPdbCodes params
            , text = params.rawPdbCodeListInput
            , placeholder = Nothing
            , label =
                Input.labelAbove []
                    (Style.h2 <|
                        text "PDB Codes (whitespace separated)"
                    )
            , spellcheck = True
            }
        , paragraph []
            [ "Found "
                ++ (Set.size params.pdbCodeList |> String.fromInt)
                ++ " PDB codes. "
                ++ (Set.size params.malformedPdbCodes |> String.fromInt)
                ++ " "
                ++ (if Set.size params.malformedPdbCodes > 1 then
                        "entries do"

                    else
                        "entry does"
                   )
                ++ " not look a like PDB code:\n"
                ++ (Set.toList params.malformedPdbCodes |> String.join " ")
                |> text
            ]
        , let
            validName =
                case params.mName of
                    Nothing ->
                        False

                    Just _ ->
                        True

            validDescription =
                case params.mDescription of
                    Nothing ->
                        False

                    Just _ ->
                        True

            complete =
                validName && validDescription && (Set.isEmpty >> not) params.pdbCodeList
          in
          Buttons.conditionalButton
            { clickMsg =
                Just ClickedDownloadPreferredSubset
            , label = text "Create Reference Set"
            , isActive = complete
            }
        ]



-- }}}
