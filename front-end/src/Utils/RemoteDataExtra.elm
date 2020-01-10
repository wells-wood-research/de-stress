module Utils.RemoteDataExtra exposing (codec)

import Codec exposing (Codec)
import RemoteData as RD exposing (RemoteData)


codec : Codec a -> Codec b -> Codec (RemoteData a b)
codec codecA codecB =
    Codec.custom
        (\fnotasked floading ffailed fsuccess value ->
            case value of
                RD.NotAsked ->
                    fnotasked

                RD.Loading ->
                    floading

                RD.Failure error ->
                    ffailed error

                RD.Success data ->
                    fsuccess data
        )
        |> Codec.variant0 "NotAsked" RD.NotAsked
        |> Codec.variant0 "Loading" RD.Loading
        |> Codec.variant1 "Failed" RD.Failure codecA
        |> Codec.variant1 "Success" RD.Success codecB
        |> Codec.buildCustom
