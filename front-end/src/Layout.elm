module Layout exposing (view)

import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Region as Region
import FeatherIcons
import Generated.Routes as Routes exposing (Route, routes)
import Style exposing (colorPalette)
import Utils.Spa as Spa


view : Spa.LayoutContext msg -> Element msg
view { page, route } =
    column [ height fill, width fill ]
        [ viewHeader route
        , el
            [ centerX
            , paddingXY 50 30
            , spacing 30
            , width (fill |> maximum 800)
            ]
            page
        ]


viewHeader : Route -> Element msg
viewHeader currentRoute =
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
            ([ viewLink currentRoute ( "Designs", routes.notFound )
             , viewLink currentRoute ( "Reference Sets", routes.notFound )
             , viewLink currentRoute ( "Specifications", routes.specifications )
             , viewLink currentRoute ( "Settings", routes.notFound )
             ]
                |> List.intersperse (el [ Font.color colorPalette.c4 ] <| text "|")
            )
        ]


viewLink : Route -> ( String, Route ) -> Element msg
viewLink currentRoute ( label, route ) =
    if currentRoute == route then
        el
            [ Font.underline
            ]
            (text label)

    else
        link
            [ alpha 0.5
            , mouseOver [ alpha 1 ]
            ]
            { label = text label
            , url = Routes.toPath route
            }
