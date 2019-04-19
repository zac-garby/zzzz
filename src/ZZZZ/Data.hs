module ZZZZ.Data where
    
import Data.List
import Control.Monad
import qualified Data.Map.Strict as M

type Value = Expr
type Error = String

-- A Result contains either an OK value - in which case a value will be returned
-- along with an environment update function - or an error value which will contain an
-- error message.
data Result a
    = Err Error
    | Ok a (Env -> Env)

instance Monad Result where
    return x = Ok x id
    
    Err msg >>= _ = Err msg
    Ok x env >>= f = case f x of
        Err msg -> Err msg
        Ok y env' -> Ok y (env' . env)

instance Applicative Result where
    pure = return
    (<*>) = ap

instance Functor Result where
    fmap = liftM

infixr 0 |||
(|||) :: Maybe a -> Error -> Result a
Nothing ||| e = Err e
Just val ||| _ = return val

data Env = Env (M.Map String Value)

instance Semigroup Env where
    (Env a) <> (Env b) = Env (a <> b)

instance Monoid Env where
    mempty = Env mempty

get :: Env -> String -> Maybe Value
get (Env env) name = M.lookup name env

set :: String -> Value -> Env -> Env
set name val (Env env) = Env $ M.insert name val env

data Strat = Lazy | Strict
    deriving (Eq, Ord, Show)

-- This is required because different arguments in a builtin might require different
-- evaluation strategies. For example, (if cond a b) will want cond to be evaluated
-- strictly but a and b to remain unevaluated.
type Strategy = [Strat]

-- This is the same data-type which is used for both parse-results and actual evaluation.
data Expr
    = Symbol String
    | Number Double
    | Str String
    | List [Expr]
    | Builtin Strategy ([Expr] -> Result Value)

instance Show Expr where
    show (Symbol x) = x
    show (Number n) = show n
    show (Str s) = "\"" ++ s ++ "\""
    show (List (Symbol "quote" : xs)) = "'" ++ intercalate " " (map show xs)
    show (List xs) = "(" ++ intercalate " " (map show xs) ++ ")"
    show (Builtin n _) = "<builtin. " ++ show n ++ " args>"

instance Eq Expr where
    (Symbol x) == (Symbol y) = x == y
    (Number x) == (Number y) = x == y
    (Str x) == (Str y) = x == y
    (List x) == (List y) = x == y
    _ == _ = False

err = Left
ok = Right