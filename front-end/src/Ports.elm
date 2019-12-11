port module Ports exposing
    ( deleteSpecification
    , getSpecification
    , setFocussedSpecification
    , storeSpecification
    )

import Codec exposing (Value)



-- {{{ Outgoing


port outgoing : { action : String, data : Value } -> Cmd msg


storeSpecification : Value -> Cmd msg
storeSpecification specValue =
    outgoing
        { action = "STORE_SPECIFICATION"
        , data = specValue
        }


getSpecification : Value -> Cmd msg
getSpecification uuidValue =
    outgoing
        { action = "GET_SPECIFICATION"
        , data = uuidValue
        }


deleteSpecification : Value -> Cmd msg
deleteSpecification uuidValue =
    outgoing
        { action = "DELETE_SPECIFICATION"
        , data = uuidValue
        }



-- }}}
-- {{{ Incoming


port setFocussedSpecification : (Value -> msg) -> Sub msg



-- }}}
