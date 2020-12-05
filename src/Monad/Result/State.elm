module Monad.Result.State exposing (..)

{-| State monad inside a Result monad
-}


type alias State e s a =
    s -> Result e ( a, s )


return : a -> State e s a
return a s =
    Ok ( a, s )


fail : e -> State e s a
fail e _ =
    Err e


get : State e s s
get s =
    Ok ( s, s )


update : (a -> s -> s) -> State e s a -> State e s a
update fn state s0 =
    Result.map (\( a, s1 ) -> ( a, fn a s1 )) (state s0)


withState : (s -> a) -> State e s a
withState fn s0 =
    Ok ( fn s0, s0 )


do : State e s a -> (a -> State e s b) -> State e s b
do toA next s0 =
    case toA s0 of
        Ok ( a, s1 ) ->
            next a s1

        Err e ->
            Err e


andThen : (a -> State e s b) -> State e s a -> State e s b
andThen next toA s0 =
    case toA s0 of
        Ok ( a, s1 ) ->
            next a s1

        Err e ->
            Err e


andMap : State e s a -> State e s (a -> b) -> State e s b
andMap toA toFn s0 =
    case toA s0 of
        Ok ( a, s1 ) ->
            case toFn s1 of
                Ok ( fn, s2 ) ->
                    Ok ( fn a, s2 )

                Err e ->
                    Err e

        Err e ->
            Err e


map : (a -> b) -> State e s a -> State e s b
map mapFn stateFn s0 =
    case stateFn s0 of
        Ok ( a, s1 ) ->
            Ok ( mapFn a, s1 )

        Err e ->
            Err e


map2 : (a -> b -> c) -> State e s a -> State e s b -> State e s c
map2 fn a b =
    return fn
        |> andMap a
        |> andMap b


map3 : (a -> b -> c -> d) -> State e s a -> State e s b -> State e s c -> State e s d
map3 fn a b c =
    return fn
        |> andMap a
        |> andMap b
        |> andMap c


mapErr : (e -> ee) -> State e s a -> State ee s a
mapErr fn state s0 =
    Result.mapError fn (state s0)



-- EVALUATE


run : s -> State e s a -> Result e ( a, s )
run s state =
    state s


finalState : s -> State e s a -> Result e s
finalState s state =
    run s state
        |> Result.map Tuple.second


finalValue : s -> State e s a -> Result e a
finalValue s state =
    run s state
        |> Result.map Tuple.first
