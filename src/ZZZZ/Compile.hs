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
compile (ExList [ExSym "lambda", ExList [ExSym p], body]) = Abstraction p <$> compile body
compile (ExList [ExSym "lambda", ExList ((ExSym p):ps), body])
    = Abstraction p <$> (compile $ ExList [ExSym "lambda", ExList ps, body])
compile (ExList [ExSym "lambda", ExList [], _]) = Left "a lambda abstraction must have at least one parameter"
compile (ExList [ExSym "lambda", ExList _, _]) = Left "a lambda abstraction's parameter list must consist of only symbols"

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