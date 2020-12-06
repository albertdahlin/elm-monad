module Monad.Writer.List exposing (..)


type alias Writer w a =
    ( a, List w )


return : a -> Writer w a
return a =
    ( a, [] )


do : Writer w a -> (a -> Writer w b) -> Writer w b
do ( a, w0 ) next =
    let
        ( b, w1 ) =
            next a
    in
    ( b, w0 ++ w1 )
