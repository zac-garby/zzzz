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
    , ("/", numOp (/))
    , ("^", numOp (**))
    , ("mod", numOp modB)
    , ("head", headB)
    , ("tail", tailB)
    , ("null", nullB)
    , ("type", typeB)
    , ("eq", eqB)
    , ("lt", ltB)
    , ("gt", gtB)
    , ("lte", lteB)
    , ("gte", gteB)
    , ("if", ifB)
    , ("inf", Number (read "Infinity")) ]

numOp :: (Double -> Double -> Double) -> Term
numOp f = [TNumber] !=> \(Number a) ->
          [TNumber] !=> \(Number b) ->
          Number (f a b)

modB :: Double -> Double -> Double
modB x y = fromIntegral $ (round x) `mod` (round y)

headB :: Term
headB = Builtin strat $ \f -> case f of
    (Application (Application (Symbol "cons" _) h) _) -> return h
    Empty -> Left "head doesn't work on empty lists"
    _ -> Left "head only works on cons-lists"
    where strat (Application (Application (Symbol "cons" _) _) _) = True
          strat _ = False

tailB :: Term
tailB = Builtin strat $ \f -> case f of
    (Application (Application (Symbol "cons" _) _) rest) -> return rest
    Empty -> Left "tail doesn't work on empty lists"
    _ -> Left "tail only works on cons-lists"
    where strat (Application (Application (Symbol "cons" _) _) _) = True
          strat _ = False

nullB :: Term
nullB = (strat, [TAny]) ==> null'
    where strat (Application (Application (Symbol "cons" _) _) _) = True
          strat _ = False
          null' Empty = true
          null' _ = false

eqB :: Term
eqB = [TAny] !=> \a ->
      [TAny] !=> \b ->
      if a == b then true
                else false

typeB :: Term
typeB = [TAny] !=> mkString . show . typeOf

ltB :: Term
ltB = [TNumber] !=> \(Number a) ->
      [TNumber] !=> \(Number b) ->
      if a < b then true
               else false

gtB :: Term
gtB = [TNumber] !=> \(Number a) ->
      [TNumber] !=> \(Number b) ->
        if a > b then true
                else false

lteB :: Term
lteB = [TNumber] !=> \(Number a) ->
       [TNumber] !=> \(Number b) ->
        if a <= b then true
                else false

gteB :: Term
gteB = [TNumber] !=> \(Number a) ->
       [TNumber] !=> \(Number b) ->
        if a >= b then true
                else false

ifB :: Term
ifB = [TAny] !=> \cond ->
      [TAny] -=> \a ->
      [TAny] -=> \b -> case cond of
          Quoted (Symbol "true" _) -> a
          _ -> b

infixr 0 -->
infixr 0 ==>
infixr 0 !=>
infixr 0 -=>

(-->) :: (Strategy, [DataType]) -> (Term -> Result Term) -> Term
(s, ts) --> f = Builtin s $ \x -> if (x `is`) `any` ts
    then f x
    else Left $ "invalid argument of type: " ++ show (typeOf x) ++ "\n\texpected one of " ++ show ts

(==>) :: (Strategy, [DataType]) -> (Term -> Term) -> Term
l ==> f = l --> (return . f)

(!=>) :: [DataType] -> (Term -> Term) -> Term
ts !=> f = (strict, ts) ==> f

(-=>) :: [DataType] -> (Term -> Term) -> Term
ts -=> f = (lazy, ts) ==> f