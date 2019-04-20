module ZZZZ.Eval
    ( eval
    , evaluate
    ) where

import ZZZZ.Parse
import ZZZZ.Data

import Data.Functor
import qualified Data.Map.Strict as M

-- | Performs one "layer" of evaluation, in the context of the given environment. The result which it returns
-- may have an environment transformer attached inside an `Ok` value, in which case the environment should be
-- updated accordingly.
eval :: Env -> Value -> Result Value
eval env (Symbol name) = get env name ||| "the symbol '" ++ name ++ "' is not defined"
eval _ n@(Number _) = return n
eval _ s@(Str _) = return s
eval _ q@(List (Symbol "quote" : _)) = return q

eval env (List [Symbol "def", Symbol name, value]) = Ok value (set name value)

eval env (List [Symbol "let", List vars, body]) = case extract vars of
    Nothing -> Err "a 'let' construct should be in the form:\n\t(let (x1 v2 x2 v2 ... xn vn) body)"
    Just v -> let (params, args) = unzip v
              in return (List ((List [(Symbol "lambda"), (List (map Symbol params)), body]) : args))
    where extract [] = Just []
          extract ((Symbol x):v:as) = (:) <$> Just (x, v) <*> extract as
          extract _ = Nothing

eval env (List ((List [(Symbol "lambda"), (List params), body]) : args)) = do
    parameters <- traverse fromSymbol params ||| "all parameters must be symbols"
    if length params == length args then
        return $ substitute (zip parameters args) body
    else
        Err $ show (length params) ++ " parameters required, but " ++ show (length args) ++ " arguments supplied"

eval env (List (Symbol "def" : _)) = Err "a 'def' construct should be in the form:\n\t(def name value)"
eval env (List (Symbol "let" : _)) = Err "a 'let' construct should be in the form:\n\t(let (x1 v2 x2 v2 ... xn vn) body)"
eval env (List ((List ((Symbol "lambda") : _) : _))) = Err "a 'lambda' expression should be in the form:\n\t(lambda (x1 x2 .. xn) body)"

eval env (List (Symbol name : args))
    = (get env name ||| "the function '" ++ name ++ "' is not defined") <&> (\x -> List (x:args))

eval env (List ((Builtin strat b) : args)) =
    if length args == length strat then do
        args' <- sequence $ zipWith (\s arg -> case s of
            Strict -> evaluate env arg
            Lazy -> return arg) strat args
        
        b args'
    else
        Err $ show (length strat) ++ " parameters required, but " ++ show (length args) ++ " arguments supplied"

eval env (List [x]) = return x

eval _ (List _) = Err "a list must either be quoted or be in the form:\n\t(f a1 a2 ... an), f ∈ (lambda ..) | symbol "

-- | Fully evaluates a value until it cannot be reduced any further, in the context of the given environment.
evaluate :: Env -> Value -> Result Value
evaluate env val = do
    if canReduce val then do
        result <- eval env val
        evaluate env result
    else
        return val

canReduce :: Value -> Bool
canReduce (Symbol _) = True
canReduce (List (Symbol "quote" : _)) = False
canReduce (List ((Symbol "lambda") : _)) = False
canReduce (List _) = True
canReduce _ = False

sub :: String -> Value -> Value -> Value
sub var val (Symbol sym) | sym == var = val
sub var val (List (Symbol "def" : name : xs)) = List $ (Symbol "def") : name : map (sub var val) xs
sub var val (List (Symbol "let" : args : xs)) = List $ (Symbol "let") : args : map (sub var val) xs
sub var val (List (Symbol "lambda" : args : xs)) = List $ (Symbol "lambda") : args : map (sub var val) xs
sub var val (List xs) = List $ map (sub var val) xs
sub _ _ o = o

substitute :: [(String, Value)] -> Value -> Value
substitute env val = foldr (uncurry sub) val env

fromSymbol :: Value -> Maybe String
fromSymbol (Symbol s) = Just s
fromSymbol _ = Nothing