module Global exposing
    ( Flags
    , Model
    , Msg(..)
    , init
    , send
    , subscriptions
    , update
    )

import Generated.Route exposing (Route)
import Specification exposing (Specification)
import Task


type alias Flags =
    ()


type alias Model =
    { specifications : List Specification }


type Msg
    = AddSpecification Specification


init : { navigate : Route -> Cmd msg } -> Flags -> ( Model, Cmd Msg, Cmd msg )
init _ _ =
    ( { specifications = [] }
    , Cmd.none
    , Cmd.none
    )


update : { navigate : Route -> Cmd msg } -> Msg -> Model -> ( Model, Cmd Msg, Cmd msg )
update _ msg model =
    case msg of
        AddSpecification spec ->
            ( { model | specifications = spec :: model.specifications }
            , Cmd.none
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- {{ Utilities


send : msg -> Cmd msg
send =
    Task.succeed >> Task.perform identity



-- }}
