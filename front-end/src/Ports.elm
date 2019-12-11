port module Ports exposing
    ( deleteDesign
    , deleteSpecification
    , getDesign
    , getSpecification
    , setFocussedDesign
    , setFocussedSpecification
    , storeDesign
    , storeSpecification
    , viewStructure
    )

import Codec exposing (Value)



-- {{{ Outgoing


port outgoing : { action : String, data : Value } -> Cmd msg



-- Design


storeDesign : Value -> Cmd msg
storeDesign designValue =
    outgoing
        { action = "STORE_DESIGN"
        , data = designValue
        }


getDesign : Value -> Cmd msg
getDesign uuidValue =
    outgoing
        { action = "GET_DESIGN"
        , data = uuidValue
        }


deleteDesign : Value -> Cmd msg
deleteDesign uuidValue =
    outgoing
        { action = "DELETE_DESIGN"
        , data = uuidValue
        }


viewStructure : Value -> Cmd msg
viewStructure pdbStringValue =
    outgoing
        { action = "VIEW_STRUCTURE"
        , data = pdbStringValue
        }



-- Specifications


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


port setFocussedDesign : (Value -> msg) -> Sub msg



-- }}}
