module Monad.Writer.String exposing (..)


type alias Writer a =
    ( a, String )


return : a -> Writer a
return a =
    ( a, "" )


do : Writer a -> (a -> Writer b) -> Writer b
do ( a, w0 ) next =
    let
        ( b, w1 ) =
            next a
    in
    ( b, w0 ++ w1 )
