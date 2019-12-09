module Layouts.Specifications exposing (view)

import Element exposing (..)
import Global


view :
    { page : Element msg
    , global : Global.Model
    }
    -> Element msg
view { page } =
    page
