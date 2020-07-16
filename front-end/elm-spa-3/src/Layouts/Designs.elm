module Layouts.Designs exposing (view)

import Element exposing (..)
import Utils.Spa as Spa


view : Spa.LayoutContext msg -> Element msg
view { page } =
    el [ centerX, width <| maximum 800 <| fill ] page

