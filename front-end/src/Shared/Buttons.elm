module Shared.Buttons exposing
    ( DangerStatus
    , alwaysActiveButton
    , conditionalButton
    , dangerousButton
    , initDangerStatus
    , isConfirmed
    , linkButton
    )

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Shared.Style as Style
import Spa.Generated.Route as Route exposing (Route)


alwaysActiveButton : { label : Element msg, clickMsg : msg, pressed : Bool } -> Element msg
alwaysActiveButton { label, clickMsg, pressed } =
    Input.button
        (Style.buttonStyle
            ++ (if pressed then
                    [ Background.color Style.colorPalette.c4
                    , Border.color Style.colorPalette.black
                    , Border.width 2
                    , Font.color Style.colorPalette.white
                    ]

                else
                    [ Background.color Style.colorPalette.c3
                    ]
               )
        )
        { onPress = Just clickMsg
        , label = label
        }


conditionalButton :
    { label : Element msg
    , clickMsg : Maybe msg
    , isActive : Bool
    }
    -> Element msg
conditionalButton { label, clickMsg, isActive } =
    if isActive then
        Input.button
            (Style.buttonStyle
                ++ [ Background.color Style.colorPalette.c3 ]
            )
            { onPress = clickMsg
            , label = label
            }

    else
        Input.button
            (Style.buttonStyle
                ++ [ Background.color Style.colorPalette.c4
                   , Border.color Style.colorPalette.black
                   , Border.width 2
                   , Font.color Style.colorPalette.white
                   ]
            )
            { onPress = Nothing
            , label = label
            }


linkButton : { label : Element msg, route : Route } -> Element msg
linkButton { label, route } =
    link
        (Style.buttonStyle
            ++ [ Background.color Style.colorPalette.c3
               ]
        )
        { url = Route.toString route, label = label }


type DangerStatus
    = Unclicked
    | Clicked HoverState
    | Confirmed


initDangerStatus : DangerStatus
initDangerStatus =
    Unclicked


isConfirmed : DangerStatus -> Bool
isConfirmed dangerStatus =
    case dangerStatus of
        Confirmed ->
            True

        _ ->
            False


type HoverState
    = Hovered
    | NotHovered


dangerousButton :
    { label : Element msg
    , confirmText : String
    , status : DangerStatus
    , dangerousMsg : DangerStatus -> msg
    }
    -> Element msg
dangerousButton { label, confirmText, status, dangerousMsg } =
    case status of
        Unclicked ->
            Input.button
                (Style.buttonStyle
                    ++ [ Background.color Style.colorPalette.red
                       ]
                )
                { onPress = Just <| dangerousMsg <| Clicked NotHovered
                , label = label
                }

        Clicked hoverState ->
            Input.button
                (Style.buttonStyle
                    ++ (case hoverState of
                            Hovered ->
                                []

                            NotHovered ->
                                [ Events.onLoseFocus <| dangerousMsg Unclicked ]
                       )
                    ++ [ Background.color Style.colorPalette.red
                       , Border.roundEach
                            { topLeft = 6
                            , topRight = 6
                            , bottomLeft = 0
                            , bottomRight = 0
                            }
                       , below <|
                            column
                                [ padding 10
                                , spacing 10
                                , width <| px 150
                                , Background.color Style.colorPalette.red
                                , Border.roundEach
                                    { topLeft = 0
                                    , topRight = 6
                                    , bottomLeft = 6
                                    , bottomRight = 6
                                    }
                                , Events.onMouseEnter <| dangerousMsg <| Clicked Hovered
                                , Events.onMouseLeave <| dangerousMsg <| Clicked NotHovered
                                , Font.color Style.colorPalette.black
                                ]
                                [ paragraph [] [ text confirmText ]
                                , row [ spacing 10 ]
                                    [ Input.button
                                        (Style.buttonStyle
                                            ++ [ focused []
                                               , Border.width 2
                                               ]
                                        )
                                        { onPress = Just <| dangerousMsg Confirmed
                                        , label = text "Yes"
                                        }
                                    , Input.button
                                        (Style.buttonStyle
                                            ++ [ focused []
                                               , Border.width 2
                                               , Input.focusedOnLoad
                                               ]
                                        )
                                        { onPress = Just <| dangerousMsg Unclicked
                                        , label = text "No"
                                        }
                                    ]
                                ]
                       ]
                )
                { onPress = Just <| dangerousMsg Unclicked
                , label = label
                }

        Confirmed ->
            Input.button
                (Style.buttonStyle
                    ++ [ Background.color Style.colorPalette.c4
                       , Font.color Style.colorPalette.c5
                       ]
                )
                { onPress = Nothing
                , label = label
                }
