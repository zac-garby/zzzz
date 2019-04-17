module ZZZZ.Eval
    ( Env (..)
    , eval
    , evaluate
    , ok
    , err
    ) where

import ZZZZ.Parse
import ZZZZ.Data

import Data.Functor
import qualified Data.Map.Strict as M

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

-- Performs one "layer" of evaluation
eval :: Env -> Value -> Result
eval env (Symbol name) = get env name ||| "the symbol '" ++ name ++ "' is not defined"
eval _ n@(Number _) = ok n
eval _ s@(Str _) = ok s
eval _ q@(List (Symbol "quote" : _)) = ok q

eval env (List (Symbol name : args))
    = (get env name ||| "the function '" ++ name ++ "' is not defined") <&> (\x -> List (x:args))
    
eval env (List ((List [(Symbol "lambda"), (List params), body]) : args)) = do
    parameters <- sequence (map fromSymbol params) ||| "all parameters must be symbols"
    if length params == length args then
        ok $ substitute (zip parameters args) body
    else
        err $ show (length params) ++ " parameters required, but " ++ show (length args) ++ " arguments supplied"

eval env (List ((Builtin strat b) : args)) =
    if length args == length strat then do
        args' <- sequence $ zipWith (\s arg -> case s of
            Strict -> evaluate env arg
            Lazy -> return arg) strat args
        
        b args'
    else
        err $ show (length strat) ++ " parameters required, but " ++ show (length args) ++ " arguments supplied"

eval env (List [x]) = ok x

eval _ (List _) = err "a list must either be quoted or be in the form:\n\t(f a1 a2 ... an), f ∈ (lambda ..) | symbol "

-- Fully evaluates a value
evaluate :: Env -> Value -> Result
evaluate env val = do    
    result <- eval env val
    
    -- traceM (show val ++ " -> " ++ show result) -- When uncommenting this, 'import Debug.Trace' in this file
        
    if canReduce result then
        evaluate env result
    else
        return result

canReduce :: Value -> Bool
canReduce (Symbol _) = True
canReduce (List (Symbol "quote" : _)) = False
canReduce (List ((Symbol "lambda") : _)) = False
canReduce (List (Symbol _ : _)) = True
canReduce (List ((List [(Symbol "lambda"), (List _), _]) : _)) = True
canReduce (List ((Builtin _ _) : _)) = True
canReduce (List [_]) = True
canReduce _ = False

sub :: String -> Value -> Value -> Value
sub var val (Symbol sym) | sym == var = val
sub var val (List (xs)) = (List (map (sub var val) xs))
sub _ _ o = o

substitute :: [(String, Value)] -> Value -> Value
substitute env val = foldr (uncurry sub) val env

fromSymbol :: Value -> Maybe String
fromSymbol (Symbol s) = Just s
fromSymbol _ = Nothing