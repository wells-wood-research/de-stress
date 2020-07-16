module Shared.Editable exposing (Editable(..), editableValue)


type Editable a
    = Editing a (Maybe a)
    | NotEditing a


editableValue : Editable a -> a
editableValue e =
    case e of
        Editing a _ ->
            a

        NotEditing a ->
            a
