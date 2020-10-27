module Shared.Stored exposing (Stored(..), getData, getUuid)


type Stored a
    = OnDisk { uuidString : String }
    | Loading { uuidString : String }
    | FailedToLoad { uuidString : String }
    | InMemory { uuidString : String, data : a }


getUuid : Stored a -> String
getUuid stored =
    case stored of
        OnDisk { uuidString } ->
            uuidString

        Loading { uuidString } ->
            uuidString

        FailedToLoad { uuidString } ->
            uuidString

        InMemory { uuidString } ->
            uuidString


getData : Stored a -> Maybe a
getData stored =
    case stored of
        InMemory { data } ->
            Just data

        _ ->
            Nothing
