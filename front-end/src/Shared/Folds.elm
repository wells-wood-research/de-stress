module Shared.Folds exposing (sectionFoldView)

import Element exposing (..)
import Element.Border as Border
import Element.Events as Events
import FeatherIcons
import Shared.Style as Style


sectionFoldView :
    { foldVisible : Bool
    , title : String
    , toggleMsg : msg
    , contentView : Element msg
    }
    -> Element msg
sectionFoldView { foldVisible, title, toggleMsg, contentView } =
    if foldVisible then
        column
            [ padding 5
            , width fill
            , Border.innerShadow
                { offset = ( 0, 0 )
                , size = 1
                , blur = 10
                , color = rgba 0.5 0.5 0.5 0.4
                }
            , Border.rounded 10
            ]
            [ row [ Events.onClick toggleMsg ]
                [ FeatherIcons.chevronsDown
                    |> Style.featherIconToElmUi
                , text <| title
                ]
            , el [ padding 10 ] contentView
            ]

    else
        row [ padding 5, width fill, Events.onClick toggleMsg ]
            [ FeatherIcons.chevronsRight
                |> Style.featherIconToElmUi
            , text <| title
            ]
