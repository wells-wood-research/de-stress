module Components.DropDown exposing (Model, Msg, init, update, view)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import FeatherIcons
import Style



---- MODEL ----


type alias Model a =
    { status : Status
    , selected : a
    }


type Status
    = Open
    | Closed


init : a -> Model a
init selected =
    { status = Closed
    , selected = selected
    }



---- UPDATE ----


type Msg a
    = ClickedOpenDropDown
    | ClickedOption a


update : Msg a -> Model a -> Model a
update msg model =
    case msg of
        ClickedOpenDropDown ->
            { model
                | status =
                    Open
            }

        ClickedOption option ->
            { model | status = Closed, selected = option }



---- VIEW ----


view : (a -> Element (Msg a)) -> List a -> Model a -> Element (Msg a)
view optionView options model =
    case model.status of
        Open ->
            openView optionView model.selected options

        Closed ->
            closedView optionView model.selected


closedView : (a -> Element (Msg a)) -> a -> Element (Msg a)
closedView optionView selected =
    el
        [ clip
        , Events.onClick ClickedOpenDropDown
        , width fill
        ]
        (row style
            [ el [ width <| px 30 ]
                (FeatherIcons.chevronDown
                    |> FeatherIcons.toHtml []
                    |> html
                )
            , optionView selected
            ]
        )


openView : (a -> Element (Msg a)) -> a -> List a -> Element (Msg a)
openView optionView selected options =
    let
        wrappedOptionView option =
            row
                (style
                    ++ [ mouseOver [ Background.color Style.colorPalette.c4 ]
                       , Border.widthEach
                            { bottom = 1, left = 1, top = 0, right = 1 }
                       , Events.onClick <| ClickedOption option
                       ]
                )
                [ el [ width <| px 30 ]
                    none
                , optionView option
                ]
    in
    el
        [ List.map wrappedOptionView options
            |> column [ width fill ]
            |> below
        , width fill
        ]
        (row
            (style
                ++ [ Events.onClick <| ClickedOption selected
                   ]
            )
            [ el [ width <| px 30 ]
                (FeatherIcons.chevronUp
                    |> FeatherIcons.toHtml []
                    |> html
                )
            , optionView selected
            ]
        )



---- STYLE ----


style : List (Attribute (Msg a))
style =
    [ paddingXY 10 8
    , width fill
    , Background.color Style.colorPalette.white
    , Border.width 1
    ]
