module Monad.State.Result exposing (..)


type alias State e s a =
    s -> ( Result e a, s )


return : a -> State e s a
return a s =
    ( Ok a, s )


fail : e -> State e s a
fail e s =
    ( Err e, s )


do : State e s a -> (a -> State e s b) -> State e s b
do sra next s0 =
    let
        ( ra, s1 ) =
            sra s0
    in
    case ra of
        Ok a ->
            next a s1

        Err e ->
            ( Err e, s1 )


run : s -> State e s a -> ( Result e a, s )
run s state =
    state s
