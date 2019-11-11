module Components.Navbar exposing (view)

import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Region as Region
import FeatherIcons
import Style exposing (colorPalette)


view : Element msg
view =
    row
        [ padding 10
        , width fill
        , Background.color colorPalette.c1
        , Font.color colorPalette.c4
        , Font.size 32
        , Font.bold
        , Region.navigation
        ]
        [ link [ Region.heading 1 ] { url = "/", label = text "DE-STRESS" }
        , row
            [ alignRight
            , spacingXY 10 0
            , Font.medium
            , Font.size 24
            ]
            ([ link [] { url = "/designs", label = text "Designs" }
             , link [] { url = "/reference-sets", label = text "Reference Sets" }
             , link [] { url = "/specifications/all", label = text "Specifiations" }
             , link []
                { url = "/settings"
                , label =
                    FeatherIcons.settings
                        |> FeatherIcons.toHtml []
                        |> html
                }
             ]
                |> List.intersperse (el [ Font.color colorPalette.c4 ] <| text "|")
            )
        ]
