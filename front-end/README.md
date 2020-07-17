# DESTRESS

## Style Guide

## Types

Type code should be organised in the following way:

1. Type definition or alias.
2. Constructors where the type is [opaque](https://medium.com/@ckoster22).
3. Type specific helper functions _e.g._
  * `(a -> b) -> a -> b`
  * `a -> b`
5. View functions _e.g._
  * a -> Element msg
  * a -> Html msg
6. Encoders, decoders or codecs.
7. Aliases (and follow organisation from step 1) _e.g._ 
    * `type alias c = a b`

