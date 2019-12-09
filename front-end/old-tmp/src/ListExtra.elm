module ListExtra exposing (unzipTriples)


unzipTriples : List ( a, b, c ) -> ( List a, List b, List c )
unzipTriples triples =
    let
        step ( x, y, z ) ( xs, ys, zs ) =
            ( x :: xs, y :: ys, z :: zs )
    in
    List.foldr step ( [], [], [] ) triples
