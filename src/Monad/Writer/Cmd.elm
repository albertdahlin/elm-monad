module Monad.Writer.Cmd exposing (..)


type alias Writer msg a =
    ( a, Cmd msg )


return : a -> Writer msg a
return a =
    ( a, Cmd.none )


do : Writer msg a -> (a -> Writer msg b) -> Writer msg b
do ( a, w0 ) next =
    let
        ( b, w1 ) =
            next a
    in
    ( b, Cmd.batch [ w0, w1 ] )
