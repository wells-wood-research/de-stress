module Pages.ReferenceSets exposing
    ( Model
    , Msg
    , page
    )

import Application.Page as Page
import Element exposing (..)
import Global
import Style exposing (h1)


type alias Model =
    {}


type Msg
    = NoOp


page =
    Page.component
        { title = title
        , init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


title : Global.Model -> Model -> String
title _ _ =
    "ReferenceSets"


init : Global.Model -> () -> ( Model, Cmd Msg, Cmd Global.Msg )
init _ _ =
    ( {}
    , Cmd.none
    , Cmd.none
    )


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update _ msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            , Cmd.none
            )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none


view : Global.Model -> Model -> Element Msg
view _ _ =
    h1 <| text "Reference Sets"
