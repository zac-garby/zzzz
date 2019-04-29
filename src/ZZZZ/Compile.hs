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

-- Special forms
compile (ExList [ExSym "lambda", ExList ps, body]) = foldr Abstraction <$> compile body <*> traverse toSym ps
    where toSym (ExSym s) = Right s
          toSym _ = Left "a lambda expression's parameter list must contain only symbols. pattern matching may be supported in the future"
compile (ExList (ExSym "lambda" : _)) = Left "a lambda expression should be in the form:\n\t(lambda (x1 x2 .. xn) body)"

-- Regular forms
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