module Shared.Folds exposing (sectionFoldView)

import Element exposing (..)
import Element.Events as Events


sectionFoldView :
    { foldVisible : Bool
    , title : String
    , toggleMsg : msg
    , contentView : Element msg
    }
    -> Element msg
sectionFoldView { foldVisible, title, toggleMsg, contentView } =
    if foldVisible then
        column []
            [ el [ Events.onClick toggleMsg ]
                (text <| title ++ " vv")
            , el [ padding 10 ] contentView
            ]

    else
        el [ Events.onClick toggleMsg ]
            (text <| title ++ " >>")
