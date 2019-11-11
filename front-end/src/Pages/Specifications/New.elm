module Pages.Specifications.New exposing
    ( Model
    , Msg
    , page
    )

import Application.Page as Page
import Element exposing (..)
import Element.Input as Input
import Global
import Specification.NewSpecification as NewSpecification exposing (NewSpecification)
import Style


type alias Model =
    { newSpecification : NewSpecification
    }


type Msg
    = UpdatedName String
    | UpdatedDescription String


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
    "New Specification"


init : Global.Model -> () -> ( Model, Cmd Msg, Cmd Global.Msg )
init _ _ =
    ( { newSpecification =
            { name = Nothing
            , description = Nothing
            , requirements = []
            }
      }
    , Cmd.none
    , Cmd.none
    )


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update _ msg model =
    let
        { newSpecification } =
            model
    in
    case msg of
        UpdatedName name ->
            if String.isEmpty name then
                ( { model | newSpecification = { newSpecification | name = Nothing } }
                , Cmd.none
                , Cmd.none
                )

            else
                ( { model | newSpecification = { newSpecification | name = Just name } }
                , Cmd.none
                , Cmd.none
                )

        UpdatedDescription description ->
            if String.isEmpty description then
                ( { model | newSpecification = { newSpecification | description = Nothing } }
                , Cmd.none
                , Cmd.none
                )

            else
                ( { model
                    | newSpecification =
                        { newSpecification
                            | description =
                                Just
                                    description
                        }
                  }
                , Cmd.none
                , Cmd.none
                )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none


view : Global.Model -> Model -> Element Msg
view _ model =
    let
        { newSpecification } =
            model
    in
    column
        [ width fill, spacing 30 ]
        [ text "Create New Specification" |> Style.h1
        , Input.text
            Style.textInputStyle
            { onChange = UpdatedName
            , text = Maybe.withDefault "" newSpecification.name
            , placeholder = Nothing
            , label =
                Input.labelAbove []
                    (Style.h2 <| text "Name")
            }
        , Input.multiline
            Style.textInputStyle
            { onChange = UpdatedDescription
            , text = Maybe.withDefault "" newSpecification.description
            , placeholder = Nothing
            , label =
                Input.labelAbove []
                    (Style.h2 <| text "Description")
            , spellcheck = True
            }
        ]
