module Pages.ReferenceSets.New exposing (Model, Msg, page)

import Element exposing (..)
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Generated.ReferenceSets.Params as Params
import Global
import ReferenceSet exposing (ReferenceSetRemoteData)
import RemoteData as RD
import Spa.Page exposing (send)
import Style
import Utils.Spa exposing (Page)


page : Page Params.New Model Msg model msg appMsg
page =
    Spa.Page.component
        { title = always "ReferenceSets - New"
        , init = always init
        , update = always update
        , subscriptions = always subscriptions
        , view = always view
        }



-- {{{ Model


type Model
    = NewHighResBiolUnit ReferenceSetRemoteData
    | NewPdbCodeList NewPdbCodeListParams


type alias NewPdbCodeListParams =
    { mName : Maybe String
    , mDescription : Maybe String
    , remoteData : ReferenceSetRemoteData
    }


defaultCodeListParams : NewPdbCodeListParams
defaultCodeListParams =
    { mName = Nothing
    , mDescription = Nothing
    , remoteData = RD.NotAsked
    }


stringFromModelType : Model -> String
stringFromModelType modelType =
    case modelType of
        NewHighResBiolUnit _ ->
            ReferenceSet.highResBiolUnits.name

        NewPdbCodeList _ ->
            "PDB Subset"


isDownloadingData : Model -> Bool
isDownloadingData model =
    let
        modelRemoteData =
            case model of
                NewHighResBiolUnit remoteData ->
                    remoteData

                NewPdbCodeList { remoteData } ->
                    remoteData
    in
    case modelRemoteData of
        RD.Loading ->
            True

        _ ->
            False



-- }}}
-- {{{ Init


init : Params.New -> ( Model, Cmd Msg, Cmd Global.Msg )
init _ =
    ( NewHighResBiolUnit RD.NotAsked
    , Cmd.none
    , Cmd.none
    )



-- }}}
-- {{{ Update


type Msg
    = UpdatedType Model
    | UpdatedName NewPdbCodeListParams String
    | UpdatedDescription NewPdbCodeListParams String
    | ClickedDownloadDefaultHighRes
    | GotHighResBiolUnitsData ReferenceSetRemoteData
    | ClickedCreateReferenceSet


update : Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update msg _ =
    case msg of
        UpdatedType newModel ->
            ( newModel
            , Cmd.none
            , Cmd.none
            )

        UpdatedName params name ->
            if String.isEmpty name then
                ( { params | mName = Nothing }
                    |> NewPdbCodeList
                , Cmd.none
                , Cmd.none
                )

            else
                ( { params | mName = Just name }
                    |> NewPdbCodeList
                , Cmd.none
                , Cmd.none
                )

        UpdatedDescription params description ->
            if String.isEmpty description then
                ( { params | mDescription = Nothing }
                    |> NewPdbCodeList
                , Cmd.none
                , Cmd.none
                )

            else
                ( { params | mDescription = Just description }
                    |> NewPdbCodeList
                , Cmd.none
                , Cmd.none
                )

        ClickedDownloadDefaultHighRes ->
            ( NewHighResBiolUnit RD.Loading
            , ReferenceSet.queryToCmd
                ReferenceSet.highResBiolUnits.query
                GotHighResBiolUnitsData
            , Cmd.none
            )

        GotHighResBiolUnitsData remoteData ->
            case remoteData of
                RD.Success metrics ->
                    ( NewHighResBiolUnit remoteData
                    , Cmd.none
                    , Global.AddNamedReferenceSet
                        ReferenceSet.highResBiolUnits.id
                        (ReferenceSet.HighResBiolUnit
                            { metrics = metrics
                            , deleteStatus = Style.Unclicked
                            }
                        )
                        |> send
                    )

                _ ->
                    ( NewHighResBiolUnit remoteData
                    , Cmd.none
                    , Cmd.none
                    )

        ClickedCreateReferenceSet ->
            ( NewHighResBiolUnit RD.NotAsked
            , Cmd.none
            , Cmd.none
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
    <|
        if isDownloadingData model then
            [ text "Downloading metric data..." ]

        else
            [ text "Create New Reference Set"
                |> Style.h1
                |> el [ centerX ]
            , row [ centerX, spacing 15 ] <|
                List.map
                    (modelTypeSelector model)
                    [ NewHighResBiolUnit RD.NotAsked, NewPdbCodeList defaultCodeListParams ]
            , case model of
                NewHighResBiolUnit _ ->
                    newHighResBiolUnitsView

                NewPdbCodeList params ->
                    newPdbCodeListView params
            ]


modelTypeSelector : Model -> Model -> Element Msg
modelTypeSelector current option =
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
        , Style.alwaysActiveButton
            { clickMsg =
                ClickedDownloadDefaultHighRes
            , labelText = "Download Reference Set"
            }
        ]


newPdbCodeListView : NewPdbCodeListParams -> Element Msg
newPdbCodeListView params =
    column [ width fill, spacing 30 ]
        [ Input.text
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
            , spellcheck = True
            }
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
                validName && validDescription
          in
          Style.conditionalButton
            { clickMsg =
                ClickedCreateReferenceSet
            , labelText = "Create Reference Set"
            , isActive = complete
            }
        ]



-- }}}
