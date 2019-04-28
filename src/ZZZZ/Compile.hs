module ZZZZ.Compile
    ( compile
    , apply
    , mkList
    ) where

import Data.Foldable (foldl')
import ZZZZ.Data

-- | Compiles an expression (which has probably just been parsed) into
-- | a term for evaluation.
compile :: Expr -> Either Error Term

compile (ExSym s) = Right $ Symbol s
compile (ExNum n) = Right $ Number n
compile (ExStr s) = Right $ mkList . map Character $ s
compile (ExChar c) = Right $ Character c
compile (ExList []) = Left "an empty list isn't allowed"
compile (ExList (x:xs)) = apply <$> compile x <*> traverse compile xs
compile (ExArr xs) = mkList <$> traverse compile xs

-- | Utilises currying to apply a lambda abstraction to multiple arguments.
apply :: Term -> [Term] -> Term
apply = foldl' Application

-- | Constructs a cons-list by repeatedly applying the cons function.
mkList :: [Term] -> Term
mkList [] = Empty
mkList (x:xs) = apply (Symbol "cons") [x, mkList xs]