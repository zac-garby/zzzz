module ZZZZ.Builtin
    ( builtins
    ) where

import ZZZZ.Data

-- | Contains all of the builtin functions, i.e. functions which cannot be defined any other way than
-- just by defining them in Haskell itself.
builtins :: [(String, Value)]
builtins = 
    [ ("+",  Builtin [Strict, Strict] (numOp "+" (+)))
    , ("*",  Builtin [Strict, Strict] (numOp "*" (*)))
    , ("-",  Builtin [Strict, Strict] (numOp "-" (-)))
    , ("/",  Builtin [Strict, Strict] (numOp "/" (/)))
    , ("if", Builtin [Strict, Lazy, Lazy] ifFn)
    , ("=",  Builtin [Strict, Strict] equal) ]

numOp :: String -> (Double -> Double -> Double) -> [Expr] -> Result Value
numOp _ f [Number x, Number y] = return $ Number (f x y)
numOp name _ _ = Err $ "only numbers supported for " ++ name

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