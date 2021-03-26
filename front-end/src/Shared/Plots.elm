port module Shared.Plots exposing (vegaPlot)

import VegaLite



-- {{{ PORTS


port vegaPlot : { plotId : String, spec : VegaLite.Spec } -> Cmd msg



-- }}}
