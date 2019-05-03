module ZZZZ.Compile
    ( compileString
    , compile
    , preprocess
    , apply
    , mkList
    ) where

import Data.Foldable (foldl')

import ZZZZ.Parse
import ZZZZ.Data

-- | Compiles a string into a term, for evaluating. It first parses it
-- | then runs it through @preprocess@ and @compile@.
compileString :: String -> Result Term
compileString str = do
    ex <- parse str ||| "invalid syntax, could not parse"
    preprocess ex >>= compile

preprocess :: Expr -> Result Expr

preprocess (ExList [ExSym "let", ExList xs, body]) = do
    (vars, vals) <- letParams xs
    return $ ExList (ExList [ExSym "lambda", ExList vars, body] : vals)
preprocess (ExList (ExSym "let" : _)) = Left "a let expression should be in the form:\n\t(let (x1 v1 x2 v2 .. xn vn) body)"

preprocess (ExList (ExSym "λ" : xs)) = Right $ ExList (ExSym "lambda" : xs)

preprocess x = Right x

-- | Compiles an expression (which has probably just been parsed) into
-- | a term for evaluation.
compile :: Expr -> Result Term

-- Special forms
compile (ExList [ExSym "lambda", ExList ps, body]) = foldr Abstraction <$> compile body <*> traverse toSym ps
    where toSym (ExSym s) = Right (Symbol s 0)
          toSym _ = Left "a lambda expression's parameter list must contain only symbols. pattern matching may be supported in the future"
compile (ExList (ExSym "lambda" : _)) = Left "a lambda expression should be in the form:\n\t(lambda (x1 x2 .. xn) body)"

-- Regular forms
compile (ExSym s) = Right $ Symbol s 0
compile (ExNum n) = Right $ Number n
compile (ExStr s) = Right $ mkList . map Character $ s
compile (ExChar c) = Right $ Character c
compile (ExList []) = Left "an empty list isn't allowed"
compile (ExList (x:xs)) = apply <$> compile x <*> traverse compile xs
compile (ExArr xs) = mkList <$> traverse compile xs

letParams :: [Expr] -> Result ([Expr], [Expr])
letParams [] = Right ([], [])
letParams (ExSym n:v:xs) = (<>) <$> Right ([ExSym n], [v]) <*> letParams xs
letParams _ = Left "a let expression should be in the form:\n\t(let (x1 v1 x2 v2 .. xn vn) body)"
