port module Ports exposing (storeSpecification)

import Codec exposing (Value)
import Json.Encode as Json


port outgoing : { action : String, data : Value } -> Cmd msg


storeSpecification : Value -> Cmd msg
storeSpecification specValue =
    outgoing
        { action = "STORE_SPECIFICATION"
        , data = specValue
        }
