module Style exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
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



-- }}
