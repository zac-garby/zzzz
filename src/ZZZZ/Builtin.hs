module ZZZZ.Builtin
    ( builtins
    ) where

import ZZZZ.Data

-- In builtin functions, it can be assumed that the correct amount
-- of elements are inside the argument list. Variadic functions are not
-- possible in lambda calculus and so they are not possible in builtin
-- functions either.

-- | A list containing all available builtin functions.
builtins :: [(String, Term)]
builtins =
    [ ("+", Builtin [Strict, Strict] addB)
    , ("-", Builtin [Strict, Strict] subB) ]

addB :: [Term] -> Result Term
addB [Number a, Number b] = return $ Number (a + b)
addB _ = Left "+ can only be applied to two numbers"

subB :: [Term] -> Result Term
subB [Number a, Number b] = return $ Number (a - b)
subB _ = Left "- can only be applied to two numbers"