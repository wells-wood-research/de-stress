module Pages.Specifications exposing (Model, Msg, Params, page)

import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Shared
import Shared.Buttons as Buttons
import Shared.Specification as Specification
import Shared.Style as Style
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



-- {{{ MODEL


type alias Model =
    { specifications : Dict String Specification.StoredSpecification
    , mSelectedSpecification : Maybe String
    }



-- }}}
-- {{{ INIT


type alias Params =
    ()


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared _ =
    case Shared.getRunState shared of
        Just runState ->
            ( { specifications = runState.specifications
              , mSelectedSpecification = runState.mSelectedSpecification
              }
            , Cmd.none
            )

        Nothing ->
            ( { specifications = Dict.empty
              , mSelectedSpecification = Nothing
              }
            , Cmd.none
            )



-- }}}
-- {{{ UPDATE


type Msg
    = ClickedSelectSpecification (Maybe String)
    | DeleteSpecification String Buttons.DangerStatus


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedSelectSpecification mSelectedSpecification ->
            ( { model | mSelectedSpecification = mSelectedSpecification }
            , Cmd.none
            )

        DeleteSpecification uuidString dangerStatus ->
            if Buttons.isConfirmed dangerStatus then
                ( { model
                    | specifications =
                        Dict.remove uuidString model.specifications
                  }
                , Specification.deleteSpecification { uuidString = uuidString }
                )

            else
                ( { model
                    | specifications =
                        Dict.update
                            uuidString
                            ((\s ->
                                { s
                                    | deleteStatus =
                                        dangerStatus
                                }
                             )
                                |> Specification.mapStoredSpecification
                                |> Maybe.map
                            )
                            model.specifications
                  }
                , Cmd.none
                )


save : Model -> Shared.Model -> Shared.Model
save { specifications, mSelectedSpecification } shared =
    Shared.mapRunState
        (\runState ->
            { runState
                | specifications = specifications
                , mSelectedSpecification = mSelectedSpecification
                , saveStateRequested = True
            }
        )
        shared


load : Shared.Model -> Model -> ( Model, Cmd Msg )
load shared model =
    case Shared.getRunState shared of
        Just { specifications, mSelectedSpecification } ->
            ( { model
                | specifications = specifications
                , mSelectedSpecification = mSelectedSpecification
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
    { title = "Specifications"
    , body = [ bodyView model ]
    }


bodyView : Model -> Element Msg
bodyView model =
    el [ centerX, width <| maximum 800 <| fill ] <|
        column
            [ width fill, spacing 30 ]
            (wrappedRow [ centerX, spacing 10 ]
                [ paragraph []
                    [ Style.h1 <|
                        text "Requirement Specifications"
                    ]
                , Buttons.linkButton
                    { route =
                        Route.Specifications__New
                    , label = text "New"
                    }
                ]
                :: (Dict.toList model.specifications
                        |> List.map
                            (\( k, v ) ->
                                ( k, Specification.storedSpecificationToStub v )
                            )
                        |> List.map (specificationStubView model.mSelectedSpecification)
                   )
            )


specificationStubView :
    Maybe String
    -> ( String, Specification.SpecificationStub )
    -> Element Msg
specificationStubView mSelectedSpecification ( uuidString, { name, description, deleteStatus } ) =
    column
        ([ padding 15
         , spacing 10
         , width fill
         , Background.color Style.colorPalette.c5
         , Border.rounded 10
         , Border.width 3
         ]
            ++ (case mSelectedSpecification of
                    Just selectedSpecification ->
                        if selectedSpecification == uuidString then
                            [ Border.color Style.colorPalette.black
                            ]

                        else
                            [ Border.color Style.colorPalette.c5
                            ]

                    Nothing ->
                        [ Border.color Style.colorPalette.c5
                        ]
               )
        )
        [ column
            [ pointer
            , spacing 10
            , width fill
            ]
            [ Style.h2 <| text name
            , paragraph [] [ text description ]
            ]
        , wrappedRow [ spacing 10, width fill ]
            [ case mSelectedSpecification of
                Just selectedSpecification ->
                    if selectedSpecification == uuidString then
                        Buttons.alwaysActiveButton
                            { label = text "Active"
                            , clickMsg =
                                ClickedSelectSpecification <|
                                    Nothing
                            , pressed = True
                            }

                    else
                        Buttons.alwaysActiveButton
                            { label = text "Select"
                            , clickMsg =
                                ClickedSelectSpecification <|
                                    Just uuidString
                            , pressed = False
                            }

                Nothing ->
                    Buttons.alwaysActiveButton
                        { label = text "Select"
                        , clickMsg =
                            ClickedSelectSpecification <|
                                Just uuidString
                        , pressed = False
                        }
            , Buttons.linkButton
                { label = text "Details"
                , route =
                    Route.Specifications__Uuid_String { uuid = uuidString }
                }
            , Buttons.dangerousButton
                { label = text "Delete"
                , confirmText = "Are you sure you want to delete this specification?"
                , status = deleteStatus
                , dangerousMsg = DeleteSpecification uuidString
                }
            ]
        ]



-- }}}
