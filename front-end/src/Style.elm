module Style exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import FeatherIcons


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
    , c4 = rgb255 220 220 220 -- #DCDED1
    , c5 = rgb255 220 220 220 -- #FAF7E4
    , white = rgb255 255 255 255
    }



-- {{ HEADINGS


h1 : Element msg -> Element msg
h1 content =
    el
        [ Font.size 32
        , Region.heading 1
        ]
        content


h2 : Element msg -> Element msg
h2 content =
    el
        [ Font.bold
        , Font.size 24
        , Region.heading 2
        ]
        content


h3 : Element msg -> Element msg
h3 content =
    el
        [ Font.italic
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


alwaysActiveButton : { labelText : String, clickMsg : msg } -> Element msg
alwaysActiveButton { labelText, clickMsg } =
    Input.button
        (buttonStyle
            ++ [ Background.color colorPalette.c3
               ]
        )
        { onPress = Just clickMsg
        , label = text labelText
        }


conditionalButton :
    { labelText : String
    , clickMsg : msg
    , isActive : Bool
    }
    -> Element msg
conditionalButton { labelText, clickMsg, isActive } =
    if isActive then
        Input.button
            (buttonStyle
                ++ [ Background.color colorPalette.c3 ]
            )
            { onPress = Just clickMsg
            , label = text labelText
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
            , label = text labelText
            }


linkButton : { labelText : String, url : String } -> Element msg
linkButton { labelText, url } =
    link
        (buttonStyle
            ++ [ Background.color colorPalette.c3
               ]
        )
        { url = url, label = text labelText }


type DangerStatus
    = Unclicked
    | Clicked
    | Confirmed


dangerousButton :
    { labelText : String
    , confirmText : String
    , status : DangerStatus
    , dangerousMsg : DangerStatus -> msg
    }
    -> Element msg
dangerousButton { labelText, confirmText, status, dangerousMsg } =
    case status of
        Unclicked ->
            Input.button
                (buttonStyle
                    ++ [ Background.color colorPalette.red
                       ]
                )
                { onPress = Just <| dangerousMsg Clicked
                , label = text labelText
                }

        Clicked ->
            column
                [ padding 10
                , spacing 10
                , width <| px 250
                , Background.color colorPalette.red
                , Border.rounded 6
                , Events.onMouseLeave <| dangerousMsg Unclicked
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
                               ]
                        )
                        { onPress = Just <| dangerousMsg Unclicked
                        , label = text "No"
                        }
                    ]
                ]

        Confirmed ->
            Input.button
                (buttonStyle
                    ++ [ Background.color colorPalette.c4
                       , Font.color colorPalette.c5
                       ]
                )
                { onPress = Nothing
                , label = text labelText
                }



-- }}
