module Monad.Continue exposing
    ( return, do, run, map
    , Continue
    )

{-|


# Continuation

@docs Continue, return, do, run, map

-}


{-| -}
type Continue r a
    = Cont ((a -> r) -> r)


{-| -}
return : a -> Continue r a
return a =
    Cont (\fn -> fn a)


{-| -}
do : Continue r a -> (a -> Continue r b) -> Continue r b
do (Cont fn) cont =
    Cont
        (\next ->
            fn
                (\a ->
                    let
                        (Cont cont2) =
                            cont a
                    in
                    cont2 next
                )
        )


{-| -}
run : (a -> r) -> Continue r a -> r
run a (Cont fn) =
    fn a


{-| -}
map : (a -> b) -> Continue r a -> Continue r b
map fn (Cont a) =
    Cont (\k -> a (k << fn))
