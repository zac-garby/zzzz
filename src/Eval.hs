module Eval
    ( whnf
    ) where

import Parse
import qualified Data.Map.Strict as M

type Value = Expr
type Error = String
type Result = Either Error Value

data Env = Env (M.Map String Value)

instance Semigroup Env where
    (Env a) <> (Env b) = Env (a <> b)

instance Monoid Env where
    mempty = Env mempty

get :: Env -> String -> Maybe Value
get (Env env) name = M.lookup name env

-- Evaluates a value to weak-head-normal-form.
whnf :: Env -> Value -> Result

-- Literals
whnf _ n@(Number _) = Right n
whnf _ s@(Str _) = Right s

-- Lists
whnf _ q@(List (Symbol "quote" : _)) = Right q
whnf env (List (Symbol name : args)) = case env `get` name of
    Nothing -> Left $ "undefined function with name '" ++ name ++ "'"
    Just (Function params val) -> Left "not yet implemented"
    Just _ -> Left $ "'" ++ name ++ "' is not a function"
whnf _ (List _) = Left "a non-quoted list must begin with a symbol referring to a function"
