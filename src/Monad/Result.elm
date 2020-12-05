module Monad.Result exposing
    ( do
    , return
    )


return : a -> Result e a
return =
    Ok


fail : e -> Result e a
fail =
    Err


do : Result e a -> (a -> Result e b) -> Result e b
do a next =
    Result.andThen next a
