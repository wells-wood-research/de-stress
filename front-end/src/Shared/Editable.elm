module Shared.Editable exposing
    ( Editable(..)
    , acceptEdit
    , cancelEdit
    , createEditable
    , editValue
    , getValue
    , startEditing
    )


type Editable a
    = Editing a (Maybe a)
    | NotEditing a


createEditable : a -> Editable a
createEditable a =
    NotEditing a


startEditing : Editable a -> Editable a
startEditing e =
    case e of
        Editing _ _ ->
            e

        NotEditing oldValue ->
            Editing oldValue Nothing


editValue : Maybe a -> Editable a -> Editable a
editValue mNewValue e =
    case e of
        Editing oldValue _ ->
            Editing oldValue mNewValue

        NotEditing _ ->
            e


acceptEdit : Editable a -> Editable a
acceptEdit e =
    case e of
        Editing _ mNewValue ->
            case mNewValue of
                Just newValue ->
                    NotEditing newValue

                Nothing ->
                    e

        NotEditing _ ->
            e


cancelEdit : Editable a -> Editable a
cancelEdit e =
    case e of
        Editing oldValue _ ->
            NotEditing oldValue

        NotEditing _ ->
            e


getValue : Editable a -> a
getValue e =
    case e of
        Editing a _ ->
            a

        NotEditing a ->
            a
