module Monad.Reader exposing (..)


type alias Reader r a =
    r -> a


return : a -> Reader r a
return a =
    \r -> a


do : Reader r a -> (a -> Reader r b) -> Reader r b
do readA next r =
    next (readA r) r


run : r -> Reader r a -> a
run r readA =
    readA r
