module Utils.Stats exposing
    ( avg, avgInt, mean, meanInt
    , median, medianInt, percentile, percentileInt
    , minimum, maximum, minmax
    , occurrences, stdDeviation, stdDeviationInt
    , atLeast, atMost
    )

{-| Statistics functions for elm


# Average

@docs avg, avgInt, mean, meanInt
@docs median, medianInt, percentile, percentileInt


# Minimum / maximum

@docs minimum, maximum, minmax


# Distribution

@docs occurrences, stdDeviation, stdDeviationInt


# Misc

@docs atLeast, atMost

-}

import Dict exposing (Dict)


{-| Alias for max, to make piped usage more intuitive
number |> atLeast 5
-}
atLeast : number -> number -> number
atLeast =
    max


{-| Alias for max, to make piped usage more intuitive
number |> atMost 5
-}
atMost : number -> number -> number
atMost =
    min


{-| Alias for List.maximum
-}
maximum : List number -> Maybe number
maximum =
    List.maximum


{-| Alias for List.minimum
-}
minimum : List number -> Maybe number
minimum =
    List.minimum


{-| Calculate the mean of a list of Float
-}
avg : List Float -> Maybe Float
avg list =
    case list of
        [] ->
            Nothing

        _ ->
            list
                |> List.foldl avgFolder ( 0, 0 )
                |> (\( c, t ) -> t / c)
                |> Just


{-| Calculate the mean of a list of Int
-}
avgInt : List Int -> Maybe Int
avgInt list =
    case list of
        [] ->
            Nothing

        _ ->
            list
                |> List.foldl avgFolder ( 0, 0 )
                |> (\( c, t ) -> t // c)
                |> Just


avgFolder : number -> ( number, number ) -> ( number, number )
avgFolder n ( count, total ) =
    ( count + 1, total + n )


{-| Alias for avg
-}
mean : List Float -> Maybe Float
mean =
    avg


{-| Alias for avgInt
-}
meanInt : List Int -> Maybe Int
meanInt =
    avgInt


{-| Get the median of a sorted list of Float
If the length of the list is even, the retun value is the average of the two
values at the middle of the list.
Returns `Nothing` if the list is empty
-}
median : List Float -> Maybe Float
median sorted =
    let
        l =
            List.length sorted

        rest =
            sorted |> List.drop ((l - 1) // 2)
    in
    if modBy 2 l == 1 then
        rest |> List.head

    else
        case rest of
            a :: b :: xs ->
                (a + b) / 2 |> Just

            _ ->
                -- List was empty
                Nothing


{-| Get the median of a sorted list of Int
If the length of the list is even, the retun value is the average of the two
values at the middle of the list.
Returns `Nothing` if the list is empty
-}
medianInt : List Int -> Maybe Int
medianInt sorted =
    let
        l =
            List.length sorted

        rest =
            sorted |> List.drop ((l - 1) // 2)
    in
    if modBy 2 l == 1 then
        rest |> List.head

    else
        case rest of
            a :: b :: xs ->
                (a + b) // 2 |> Just

            _ ->
                -- List was empty
                Nothing


{-| Get minimum and maximum from list
Returns `Nothing` if list is empty
-}
minmax : List number -> Maybe ( number, number )
minmax list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            xs |> List.foldl minmaxFolder ( x, x ) |> Just


minmaxFolder n ( low, high ) =
    ( min low n, max high n )


{-| Get a `Dict` containing the numbers from the list as keys
and the number of occurrences for each number as value
-}
occurrences : List number -> Dict number Int
occurrences list =
    list |> List.foldl countFolder Dict.empty


countFolder n dict =
    dict |> Dict.update n (\c -> c |> Maybe.withDefault 0 |> (+) 1 |> Just)


{-| Get the element at a position in percent from a list
If the percentage doesn't exactly match an element the value is interpolated
from the two closest elements
-}
percentile : Float -> List Float -> Maybe Float
percentile p sorted =
    let
        l =
            List.length sorted

        pos =
            (toFloat l - 1) * p

        weight =
            pos - toFloat (floor pos)

        rest =
            sorted |> List.drop (floor pos)
    in
    case rest of
        a :: b :: xs ->
            (a * (1 - weight)) + (b * weight) |> Just

        a :: [] ->
            Just a

        [] ->
            -- List was empty
            Nothing


{-| Get the element at a position in percent from a list
If the percentage doesn't exactly match an element the value is interpolated
from the two closest elements
-}
percentileInt : Float -> List Int -> Maybe Int
percentileInt p sorted =
    let
        l =
            List.length sorted

        pos =
            (toFloat l - 1) * p

        weight =
            pos - toFloat (floor pos)

        rest =
            sorted |> List.drop (floor pos)
    in
    case rest of
        a :: b :: xs ->
            ((toFloat a * (1 - weight)) + (toFloat b * weight))
                -- use truncate for same behaviour as int division (`//`)
                |> truncate
                |> Just

        a :: [] ->
            Just a

        [] ->
            -- List was empty
            Nothing


{-| Get the standard deviation of a population of Float
-}
stdDeviation : List Float -> Maybe Float
stdDeviation population =
    case mean population of
        Nothing ->
            Nothing

        Just m ->
            let
                ( count, total ) =
                    population
                        |> List.foldl
                            (\x ( n, sum ) ->
                                ( n + 1, sum + ((x - m) ^ 2) )
                            )
                            ( 0, 0 )
            in
            Just <| sqrt (total / count)


{-| Get the standard deviation of a population of Int
-}
stdDeviationInt : List Int -> Maybe Int
stdDeviationInt population =
    case meanInt population of
        Nothing ->
            Nothing

        Just m ->
            let
                ( count, total ) =
                    population
                        |> List.foldl
                            (\x ( n, sum ) ->
                                ( n + 1, sum + ((x - m) ^ 2) )
                            )
                            ( 0, 0 )
            in
            sqrt (toFloat total / toFloat count) |> round |> Just
