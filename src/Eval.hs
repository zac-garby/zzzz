module Eval
    ( Env (..)
    , eval
    , evaluate
    ) where

import Parse
import Data.Functor
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

infixr 0 |||
(|||) :: Maybe a -> b -> Either b a
Nothing ||| e = Left e
Just val ||| _ = Right val

err = Left
ok = Right

-- Performs one "layer" of evaluation
eval :: Env -> Value -> Result
eval env (Symbol name) = get env name ||| "the symbol '" ++ name ++ "' is not defined"
eval _ n@(Number _) = Right n
eval _ s@(Str _) = Right s
eval _ q@(List (Symbol "quote" : _)) = Right q

eval env (List (Symbol name : args))
    = (get env name ||| "the function '" ++ name ++ "' is not defined") <&> (\x -> List (x:args))
    
eval env (List ((List [(Symbol "lambda"), (List params), body]) : args)) = do
    parameters <- sequence (map fromSymbol params) ||| "all parameters must be symbols"
    if length params == length args then
        ok $ substitute (zip parameters args) body
    else
        err $ show (length params) ++ " parameters required, but " ++ show (length args) ++ " arguments supplied"
        
eval _ (List _) = err "a list must either be quoted or be in the form:\n\t(f a1 a2 ... an), f ∈ (lambda ..) | symbol "

-- Fully evaluates a value
evaluate :: Env -> Value -> Result
evaluate env val = do    
    result <- eval env val
    result' <- eval env result
    
    -- traceM (show val ++ " -> " ++ show result) -- When uncommenting this, 'import Debug.Trace' in this file
        
    if result == result' then
        return result
    else
        evaluate env result

sub :: String -> Value -> Value -> Value
sub var val (Symbol sym) | sym == var = val
sub var val (List (x : args)) | x /= Symbol "quote" = (List (x : map (sub var val) args))
sub _ _ o = o

substitute :: [(String, Value)] -> Value -> Value
substitute env val = foldr (uncurry sub) val env

fromSymbol :: Value -> Maybe String
fromSymbol (Symbol s) = Just s
fromSymbol _ = Nothing