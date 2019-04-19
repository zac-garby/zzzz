module ZZZZ.Builtin
    ( builtins
    ) where

import ZZZZ.Data

builtins :: [(String, Value)]
builtins = 
    [ ("+",  Builtin [Strict, Strict] add)
    , ("if", Builtin [Strict, Lazy, Lazy] ifFn)
    , ("=",  Builtin [Strict, Strict] equal) ]

add :: [Expr] -> Result Value
add [Number x, Number y] = return $ Number (x + y)
add _ = Err "only numbers can be added using +"

ifFn :: [Expr] -> Result Value
ifFn [(List [(Symbol "quote"), Symbol "true"]), a, _] = return a
ifFn [(List [(Symbol "quote"), Symbol "false"]), _, b] = return b
ifFn _ = Err "an if-expression should be in the form:\n\t(if cond a b), cond ∈ ['true, 'false]"

equal :: [Expr] -> Result Value
equal [x, y] = return $
    if x == y then
        List [(Symbol "quote"), Symbol "true"]
    else
        List [(Symbol "quote"), Symbol "false"]
equal _ = Err "this shouldn't be reached... report it as a bug"