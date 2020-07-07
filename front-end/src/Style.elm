module Style exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import FeatherIcons


featherIconToElmUi : FeatherIcons.Icon -> Element msg
featherIconToElmUi =
    FeatherIcons.toHtml [] >> html


colorPalette :
    { black : Color
    , red : Color
    , c1 : Color
    , c2 : Color
    , c3 : Color
    , c4 : Color
    , c5 : Color
    , white : Color
    }
colorPalette =
    { black = rgb255 0 0 0
    , red = rgb255 202 107 104
    , c1 = rgb255 76 120 168 -- #626368
    , c2 = rgb255 160 160 160 -- #FFFAB6
    , c3 = rgb255 217 184 97 -- #EAD3A7
    , c4 = rgb255 190 190 190 -- #DCDED1
    , c5 = rgb255 220 220 220 -- #FAF7E4
    , white = rgb255 255 255 255
    }



-- {{ HEADINGS


h1 : Element msg -> Element msg
h1 content =
    el
        [ paddingEach { top = 12, bottom = 8, left = 0, right = 0 }
        , Font.bold
        , Font.size 32
        , Region.heading 1
        ]
        content


h2 : Element msg -> Element msg
h2 content =
    el
        [ paddingEach { top = 12, bottom = 8, left = 0, right = 0 }
        , Font.size 26
        , Region.heading 2
        ]
        content


h3 : Element msg -> Element msg
h3 content =
    el
        [ paddingEach { top = 12, bottom = 8, left = 0, right = 0 }
        , Font.italic
        , Font.size 22
        , Region.heading 3
        ]
        content



-- }}
-- {{ BORDERS


defaultBorder : List (Attribute msg)
defaultBorder =
    [ Border.rounded 10
    , Border.width 2
    ]



-- }}
-- {{ INPUT


textInputStyle : List (Attribute msg)
textInputStyle =
    [ padding 8
    , Border.color colorPalette.c1
    ]
        ++ defaultBorder


buttonStyle : List (Attribute msg)
buttonStyle =
    [ paddingEach { top = 12, bottom = 8, left = 8, right = 8 }
    , Border.rounded 6
    ]


alwaysActiveButton : { label : Element msg, clickMsg : msg, pressed : Bool } -> Element msg
alwaysActiveButton { label, clickMsg, pressed } =
    Input.button
        (buttonStyle
            ++ (if pressed then
                    [ Background.color colorPalette.c4
                    , Border.color colorPalette.black
                    , Border.width 2
                    , Font.color colorPalette.white
                    ]

                else
                    [ Background.color colorPalette.c3
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
            (buttonStyle
                ++ [ Background.color colorPalette.c3 ]
            )
            { onPress = clickMsg
            , label = label
            }

    else
        Input.button
            (buttonStyle
                ++ [ Background.color colorPalette.c4
                   , Border.color colorPalette.black
                   , Border.width 2
                   , Font.color colorPalette.white
                   ]
            )
            { onPress = Nothing
            , label = label
            }


linkButton : { label : Element msg, url : String } -> Element msg
linkButton { label, url } =
    link
        (buttonStyle
            ++ [ Background.color colorPalette.c3
               ]
        )
        { url = url, label = label }


type DangerStatus
    = Unclicked
    | Clicked HoverState
    | Confirmed


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
                (buttonStyle
                    ++ [ Background.color colorPalette.red
                       ]
                )
                { onPress = Just <| dangerousMsg <| Clicked NotHovered
                , label = label
                }

        Clicked hoverState ->
            Input.button
                (buttonStyle
                    ++ (case hoverState of
                            Hovered ->
                                []

                            NotHovered ->
                                [ Events.onLoseFocus <| dangerousMsg Unclicked ]
                       )
                    ++ [ Background.color colorPalette.red
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
                                , Background.color colorPalette.red
                                , Border.roundEach
                                    { topLeft = 0
                                    , topRight = 6
                                    , bottomLeft = 6
                                    , bottomRight = 6
                                    }
                                , Events.onMouseEnter <| dangerousMsg <| Clicked Hovered
                                , Events.onMouseLeave <| dangerousMsg <| Clicked NotHovered
                                , Font.color colorPalette.black
                                ]
                                [ paragraph [] [ text confirmText ]
                                , row [ spacing 10 ]
                                    [ Input.button
                                        (buttonStyle
                                            ++ [ focused []
                                               , Border.width 2
                                               ]
                                        )
                                        { onPress = Just <| dangerousMsg Confirmed
                                        , label = text "Yes"
                                        }
                                    , Input.button
                                        (buttonStyle
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
                (buttonStyle
                    ++ [ Background.color colorPalette.c4
                       , Font.color colorPalette.c5
                       ]
                )
                { onPress = Nothing
                , label = label
                }



-- }}
