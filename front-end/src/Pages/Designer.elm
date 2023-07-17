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
import Html
import Html.Attributes as HAtt
import Shared
import Shared.Buttons as Buttons
import Shared.Style as Style
import Spa.Document exposing (Document)
import Spa.Page as Page exposing (Page)
import Spa.Url as Url exposing (Url)
import Task


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
    | ProbabilityDist ProbabilityDistData
    | NewDesigns {}
    | FailedToLoad


type alias ProbabilityDistData =
    { fileName : String
    , pdbString : String
    , probabilityDist : List (Dict String Float)
    }



-- Next task:
--    Make a button and a message to go from the structure
--    to a structure with a prediction for each amino acid.
--    Look at the format for the predictions that is generated
--    by the streamlit app, and mock that up for a file here.
-- Beyond that:
--    * generate the top predicted sequence from the dist
--    * sample from the dist using Leo's monte carlo sampling ported to elm
--    * use HTTP to get a predicted structure from ESMFold server (talk to me!)
--         curl -X POST --data "KVFGRCELAAAMKRHGLDNYRGYSLGNWVCAAKFESNFNTQATNRNTDGSTDYGILQINSRWWCNDGRTPGSRNLCNIPCSALLSSDITASVNCAKKIVSDGNGMNAWVAWRNRCKGTDVQAWIRGCRL" https://api.esmatlas.com/foldSequence/v1/pdb/


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

        StructureFileSelected file ->
            ( model
            , Task.perform
                (StructureLoaded (File.name file))
                (File.toString file)
            )

        StructureLoaded name fileString ->
            ( Structure name fileString
            , Cmd.none
            )


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
    column
        [ width fill
        , spacing 20
        ]
        [ header model
        , el [ centerX, width fill ] <|
            case model of
                AwaitingFile ->
                    paragraph [] [ text "Click load to upload a template PDB file." ]

                Loading ->
                    paragraph [] [ text "Loading PDB file..." ]

                Structure fileName fileString ->
                    loadedStructureView fileName fileString

                _ ->
                    text "oops"
        ]


header : Model -> Element Msg
header model =
    let
        ( label, isActive ) =
            case model of
                AwaitingFile ->
                    ( "Load", True )

                Loading ->
                    ( "Loading", False )

                _ ->
                    ( "Load New", True )
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


loadedStructureView : String -> String -> Element Msg
loadedStructureView fileName fileString =
    column
        [ width fill ]
        [ paragraph [ centerX ]
            [ "Loaded structure \""
                ++ fileName
                ++ "\""
                |> text
            ]
        , sectionColumn
            [ Style.h2 <| text "Structure"
            , el [ height <| px 400, width fill, padding 5, Border.width 1 ]
                (Html.node "ngl-viewer"
                    [ HAtt.id "viewer"
                    , HAtt.style "width" "100%"
                    , HAtt.style "height" "100%"
                    , HAtt.attribute "pdb-string" fileString
                    ]
                    []
                    |> html
                )
            ]
        , el [] (text fileString)
        ]


sectionColumn : List (Element msg) -> Element msg
sectionColumn =
    column [ spacing 12, width fill ]
