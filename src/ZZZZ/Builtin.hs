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
    [ ("+", addB) ]

addB :: Term
addB = Builtin Strict $ \x -> case x of
    Number a -> return $ Builtin Strict $ \y -> case y of
        Number b -> return $ Number (a + b)
        _ -> Left "+ can only accept number arguments"
    _ -> Left "+ can only accept number arguments"
