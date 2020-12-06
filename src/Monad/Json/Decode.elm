module Monad.Json.Decode exposing (..)

import Json.Decode exposing (Decoder)


return : a -> Decoder a
return =
    Json.Decode.succeed


do : Decoder a -> (a -> Decoder b) -> Decoder b
do ma next =
    Json.Decode.andThen next ma
