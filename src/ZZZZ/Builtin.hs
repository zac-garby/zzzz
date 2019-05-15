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
    [ ("+", numOp (+))
    , ("-", numOp (-))
    , ("*", numOp (*))
    , ("/", numOp (/)) ]

numOp :: (Double -> Double -> Double) -> Term
numOp f = [TNumber] !=> \(Number a) ->
          [TNumber] !=> \(Number b) ->
          Number (f a b)

infixr 0 -->
infixr 0 ==>
infixr 0 !=>
infixr 0 -=>

(-->) :: (Strat, [DataType]) -> (Term -> Result Term) -> Term
(s, ts) --> f = Builtin s $ \x -> if typeOf x `elem` ts
    then f x
    else Left $ "invalid argument of type: " ++ show (typeOf x) ++ "\n\texpected one of " ++ show ts

(==>) :: (Strat, [DataType]) -> (Term -> Term) -> Term
l ==> f = l --> (return . f)

(!=>) :: [DataType] -> (Term -> Term) -> Term
ts !=> f = (Strict, ts) ==> f

(-=>) :: [DataType] -> (Term -> Term) -> Term
ts -=> f = (Lazy, ts) ==> f