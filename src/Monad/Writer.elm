module Monad.Writer exposing (..)


type alias Writer monoid a =
    ( a, monoid )


do : (w -> w -> w) -> Writer w a -> (a -> Writer w b) -> Writer w b
do append ( a, w0 ) next =
    let
        ( b, w1 ) =
            next a
    in
    ( b, append w0 w1 )
