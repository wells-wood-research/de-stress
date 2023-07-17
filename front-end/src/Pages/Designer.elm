module Pages.Designer exposing (Model, Msg, Params, page)

import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import File exposing (File)
import File.Select as FileSelect
import Shared
import Shared.Buttons as Buttons
import Shared.Style as Style
import Spa.Document exposing (Document)
import Spa.Page as Page exposing (Page)
import Spa.Url as Url exposing (Url)


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



-- INIT


type alias Params =
    ()


type Model
    = AwaitingFile
    | Loading
    | Structure String String
    | Prediction PredictionData
    | FailedToLoad


type alias PredictionData =
    { fileName : String
    , pdbString : String
    , probabilityDist : List (Dict String Float)
    }


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared { params } =
    ( AwaitingFile, Cmd.none )



-- UPDATE


type Msg
    = StructureRequested
    | StructureFileSelected File
    | StructureLoaded String String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StructureRequested ->
            ( Loading, structureRequested )

        _ ->
            Debug.todo "Finish these messages"


structureRequested : Cmd Msg
structureRequested =
    FileSelect.file [ "*/*" ] StructureFileSelected


save : Model -> Shared.Model -> Shared.Model
save model shared =
    shared


load : Shared.Model -> Model -> ( Model, Cmd Msg )
load shared model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Designer"
    , body = [ bodyView model ]
    }


bodyView : Model -> Element Msg
bodyView model =
    let
        ( label, isActive ) =
            case model of
                AwaitingFile ->
                    ( "Load", True )

                Loading ->
                    ( "Loading", False )

                _ ->
                    ( "Load", True )
    in
    wrappedRow [ centerX, spacing 10 ]
        [ paragraph [] [ Style.h1 <| text "Designer" ]
        , row [ spacing 10 ]
            [ Buttons.conditionalButton
                { label = text label
                , clickMsg = Just StructureRequested
                , isActive = isActive
                }
            ]
        ]
