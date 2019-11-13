module Global exposing
    ( Flags
    , Model
    , Msg(..)
    , init
    , send
    , subscriptions
    , update
    )

import Generated.Route as TopRoute
import Generated.Route.Specifications as SpecRoute
import Specification exposing (Specification)
import Style
import Task


type alias Flags =
    ()


type alias Model =
    { specifications : List Specification }


type Msg
    = AddSpecification Specification
    | DeleteSpecification Int Style.DangerStatus



-- {{{ Init


init : { navigate : TopRoute.Route -> Cmd msg } -> Flags -> ( Model, Cmd Msg, Cmd msg )
init _ _ =
    ( { specifications = [] }
    , Cmd.none
    , Cmd.none
    )



-- }}}
-- {{{ Update


update : { navigate : TopRoute.Route -> Cmd msg } -> Msg -> Model -> ( Model, Cmd Msg, Cmd msg )
update { navigate } msg model =
    case msg of
        AddSpecification spec ->
            ( { model | specifications = spec :: model.specifications }
            , Cmd.none
            , navigate <|
                TopRoute.Specifications (SpecRoute.All ())
            )

        DeleteSpecification index dangerStatus ->
            ( case dangerStatus of
                Style.Confirmed ->
                    { model
                        | specifications =
                            List.indexedMap Tuple.pair model.specifications
                                |> List.filter
                                    (\( i, _ ) -> i /= index)
                                |> List.map Tuple.second
                    }

                _ ->
                    { model
                        | specifications =
                            List.indexedMap
                                (\i s ->
                                    if i == index then
                                        { s
                                            | deleteStatus =
                                                dangerStatus
                                        }

                                    else
                                        s
                                )
                                model.specifications
                    }
            , Cmd.none
            , Cmd.none
            )



-- }}}
-- {{{ Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- }}}
-- {{{ Utilities


send : msg -> Cmd msg
send =
    Task.succeed >> Task.perform identity



-- }}}
