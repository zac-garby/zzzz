module ZZZZ.Data where
    
import Data.List
import Control.Monad
import qualified Data.Map.Strict as M

-- | Represents an expression which is used in evaluation, as opposed to one which is
-- a result of a parse. In reality, though, expressions and values are the same type.
type Value = Expr

-- | Can be returned when evaluating a value. It will be used to indicate to the user
-- which error happened.
type Error = String

-- | A Result contains either an OK value - in which case a value will be returned
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

-- | @x ||| err@ will return an error result with the given error message if @x@ is @Nothing@,
-- and otherwise will return the value inside @x@, wrapped in @Ok@.
(|||) :: Maybe a -> Error -> Result a
Nothing ||| e = Err e
Just val ||| _ = return val

-- | Short for "environment", an @Env@ maps names to values to be retrieved when evaluating a symbol.
data Env = Env (M.Map String Value)

instance Semigroup Env where
    (Env a) <> (Env b) = Env (a <> b)

instance Monoid Env where
    mempty = Env mempty

-- | Returns the corresponding value to the given name inside an environment.
get :: Env -> String -> Maybe Value
get (Env env) name = M.lookup name env

-- | Sets the value of the given name to some value, in an environment.
set :: String -> Value -> Env -> Env
set name val (Env env) = Env $ M.insert name val env

-- | Specifies the evaluation strategy of a particular parameter of a builtin function.
data Strat = Lazy | Strict | WHNF
    deriving (Eq, Ord, Show)

-- | Specifies the evaluation strategy of each of a builtin's parameters.
-- This is required because different arguments in a builtin might require different
-- evaluation strategies. For example, (if cond a b) will want cond to be evaluated
-- strictly but a and b to remain unevaluated.
type Strategy = [Strat]

-- | This is the same data-type which is used for both parse-results and actual evaluation.
-- Generally, when it is referred to as @Expr@, it will be a parse result.
data Expr
    = Symbol String
    | Number Double
    | Str String
    | List [Expr]
    | Array [Expr]
    | Builtin Strategy ([Expr] -> Result Value)

instance Show Expr where
    show (Symbol x) = x
    show (Number n) = if integer n then show (round n) else show n
        where integer n = n == fromInteger (round n)
    show (Str s) = "\"" ++ s ++ "\""
    show (List [Symbol "lambda", args, body]) = "(lambda " ++ show args ++ " " ++ trim (show body) ++ ")"
        where trim xs | length xs > 32 = (take 27 xs) ++ " ... " ++ takeWhile (==')') (reverse xs)
                      | otherwise = xs
    show (List (Symbol "quote" : xs)) = "'" ++ intercalate " " (map show xs)
    show (List xs) = "(" ++ intercalate " " (map show xs) ++ ")"
    show (Array xs) = "[" ++ intercalate " " (map show xs) ++ "]"
    show (Builtin n _) = "<builtin. " ++ show (length n) ++ " args>"

instance Eq Expr where
    (Symbol x) == (Symbol y) = x == y
    (Number x) == (Number y) = x == y
    (Str x) == (Str y) = x == y
    (List x) == (List y) = x == y
    (Array x) == (Array y) = x == y
    _ == _ = False
