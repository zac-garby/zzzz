module ZZZZ.Compile
    ( compile
    , apply
    , mkList
    ) where

import Data.Foldable (foldl')
import ZZZZ.Data

-- | Compiles an expression (which has probably just been parsed) into
-- | a term for evaluation.
compile :: Expr -> Term

compile (ExSym s) = Symbol s
compile (ExNum n) = Number n
compile (ExStr s) = mkList . map Character $ s
compile (ExChar c) = Character c

-- | Utilises currying to apply a lambda abstraction to multiple arguments.
apply :: Term -> [Term] -> Term
apply = foldl' Application

-- | Constructs a cons-list by repeatedly applying the cons function.
mkList :: [Term] -> Term
mkList [] = Empty
mkList (x:xs) = apply (Symbol "cons") [x, mkList xs]