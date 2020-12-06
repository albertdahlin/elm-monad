module Monad.Maybe exposing (..)


return : a -> Maybe a
return =
    Just


do : Maybe a -> (a -> Maybe b) -> Maybe b
do ma next =
    case ma of
        Just a ->
            next a

        Nothing ->
            Nothing
