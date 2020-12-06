module Monad.State exposing (..)


type alias State s a =
    s -> ( a, s )


return : a -> State s a
return a =
    \s -> ( a, s )


do : State s a -> (a -> State s b) -> State s b
do stateA next s0 =
    let
        ( a, s1 ) =
            stateA s0
    in
    next a s1


get : State s s
get s =
    ( s, s )


update : (a -> s -> s) -> State s a -> State s a
update fn state s0 =
    let
        ( a, s1 ) =
            state s0
    in
    ( a, fn a s1 )


withState : (s -> a) -> State s a
withState fn s =
    ( fn s, s )


andThen : (a -> State s b) -> State s a -> State s b
andThen next stateA s0 =
    let
        ( a, s1 ) =
            stateA s0
    in
    next a s1


andMap : State s a -> State s (a -> b) -> State s b
andMap stateA stateAB s0 =
    let
        ( a, s1 ) =
            stateA s0

        ( fn, s2 ) =
            stateAB s1
    in
    ( fn a, s2 )


map : (a -> b) -> State s a -> State s b
map mapFn stateA s0 =
    let
        ( a, s1 ) =
            stateA s0
    in
    ( mapFn a, s1 )


map2 : (a -> b -> c) -> State s a -> State s b -> State s c
map2 mapFn a b =
    return mapFn
        |> andMap a
        |> andMap b


map3 : (a -> b -> c -> d) -> State s a -> State s b -> State s c -> State s d
map3 mapFn a b c =
    return mapFn
        |> andMap a
        |> andMap b
        |> andMap c


combine : List (State s a) -> State s (List a)
combine list s0 =
    List.foldl identity s0 list


run : s -> State s a -> ( a, s )
run s state =
    state s


finalState : s -> State s a -> s
finalState s state =
    run s state
        |> Tuple.second


finalValue : s -> State s a -> a
finalValue s state =
    run s state
        |> Tuple.first
