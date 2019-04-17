module ZZZZ.Builtin
    ( builtins
    ) where

import ZZZZ.Data

builtins :: [(String, Value)]
builtins = 
    [ ("+",  Builtin [Strict, Strict] add)
    , ("if", Builtin [Strict, Lazy, Lazy] ifFn)
    , ("=",  Builtin [Strict, Strict] equal) ]

add :: [Expr] -> Result
add [Number x, Number y] = ok $ Number (x + y)
add _ = err "only numbers can be added using +"

ifFn :: [Expr] -> Result
ifFn [(List [(Symbol "quote"), Symbol "true"]), a, _] = ok a
ifFn [(List [(Symbol "quote"), Symbol "false"]), _, b] = ok b
ifFn _ = err "an if-expression should be in the form:\n\t(if cond a b), cond ∈ ['true, 'false]"

equal :: [Expr] -> Result
equal [x, y] = ok $
    if x == y then
        List [(Symbol "quote"), Symbol "true"]
    else
        List [(Symbol "quote"), Symbol "false"]
equal _ = err "this shouldn't be reached... report it as a bug"