module Shared.Style exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
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


pageWidths : { singleColumn : Attribute msg }
pageWidths =
    { singleColumn = width <| maximum 800 <| fill
    }



-- {{{ HEADINGS


h1 : Element msg -> Element msg
h1 content =
    paragraph
        [ paddingEach { top = 12, bottom = 8, left = 0, right = 0 }
        , Font.bold
        , Font.size 32
        , Region.heading 1
        ]
        [ content ]


h2 : Element msg -> Element msg
h2 content =
    paragraph
        [ paddingEach { top = 12, bottom = 8, left = 0, right = 0 }
        , Font.size 26
        , Region.heading 2
        ]
        [ content ]


h3 : Element msg -> Element msg
h3 content =
    paragraph
        [ paddingEach { top = 12, bottom = 8, left = 0, right = 0 }
        , Font.italic
        , Font.size 22
        , Region.heading 3
        ]
        [ content ]



-- }}}
-- {{{ BORDERS


defaultBorder : List (Attribute msg)
defaultBorder =
    [ Border.rounded 10
    , Border.width 2
    ]



-- }}}
-- {{{ INPUT


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


linkStyle : List (Attribute msg)
linkStyle =
    [ Font.underline
    ]



-- }}}
-- {{{ PROGRESS BAR


progressBar : { max : Int, current : Int } -> Element msg
progressBar { max, current } =
    let
        segmentView filled segmentNumber =
            let
                rounded =
                    if segmentNumber == 1 then
                        Border.roundEach
                            { topLeft = 10
                            , topRight = 0
                            , bottomLeft = 10
                            , bottomRight = 0
                            }

                    else if segmentNumber == max then
                        Border.roundEach
                            { topLeft = 0
                            , topRight = 10
                            , bottomLeft = 0
                            , bottomRight = 10
                            }

                    else
                        Border.roundEach
                            { topLeft = 0
                            , topRight = 0
                            , bottomLeft = 0
                            , bottomRight = 0
                            }
            in
            if segmentNumber <= filled then
                el
                    [ height fill
                    , width <| fillPortion 1
                    , Background.color colorPalette.c1
                    , rounded
                    ]
                    none

            else
                el
                    [ height fill
                    , width <| fillPortion 1
                    , rounded
                    ]
                    none
    in
    row
        ([ height <| px 20
         , width fill
         ]
            ++ defaultBorder
        )
        (List.map (segmentView current) (List.range 1 max))



-- }}}
