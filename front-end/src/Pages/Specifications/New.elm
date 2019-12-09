module Pages.Specifications.New exposing (Model, Msg, page)

import Spa.Page
import Element exposing (..)
import Generated.Specifications.Params as Params
import Global
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



-- INIT


type alias Model =
    {}


init : Params.New -> ( Model, Cmd Msg, Cmd Global.Msg )
init _ =
    ( {}
    , Cmd.none
    , Cmd.none
    )



-- UPDATE


type Msg
    = Msg


update : Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update msg model =
    ( model
    , Cmd.none
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Element Msg
view model =
    text "Specifications.New"