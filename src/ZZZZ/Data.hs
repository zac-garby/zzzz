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

-- | @Expr@ values are output from the parser and will be transformed into thunks for evaluation.
data Expr
    = ExSym String
    | ExNum Double
    | ExStr String
    | ExChar Char
    | ExList [Expr]
    | ExArr [Expr]

instance Show Expr where
    show (ExSym x) = x
    show (ExNum n) = if integer n then show (round n) else show n
        where integer n = n == fromInteger (round n)
    show (ExStr s) = "\"" ++ s ++ "\""
    show (ExChar c) = show c
    show (ExList [ExSym "lambda", args, body]) = "(lambda " ++ show args ++ " " ++ trim (show body) ++ ")"
        where trim xs | length xs > 32 = (take 27 xs) ++ " ... " ++ takeWhile (==')') (reverse xs)
                      | otherwise = xs
    show (ExList (ExSym "quote" : xs)) = "'" ++ intercalate " " (map show xs)
    show (ExList xs) = "(" ++ intercalate " " (map show xs) ++ ")"
    show (ExArr xs) = "[" ++ intercalate " " (map show xs) ++ "]"

instance Eq Expr where
    (ExSym x) == (ExSym y) = x == y
    (ExNum x) == (ExNum y) = x == y
    (ExStr x) == (ExStr y) = x == y
    (ExChar x) == (ExChar y) = x == y
    (ExList x) == (ExList y) = x == y
    (ExArr x) == (ExArr y) = x == y
    _ == _ = False

-- | @Term@s are the data type which is evaluated by the interpreter. They are more
-- | akin to lambda calculus than lisp s-expressions in their structure. They may be
-- | evaluated fully, e.g. a number or a string, or they may be unevaluated, like
-- | a lambda application.
data Term
    = Symbol String -- ^ Usually a variable name when not quoted
    | Number Double -- ^ Holds a floating-point number
    | Character Char -- ^ A single character value
    | Quoted Term -- ^ A quoted term, signifying that it shouldn't be evaluated
    | Empty -- ^ An empty array for the end of cons-lists
    | Abstraction String Term -- ^ A lambda abstraction, i.e. a function
    | Application Term Term -- ^ A lambda application, i.e. a function call

instance Show Term where
    show (Symbol x) = x
    show (Number n) = if integer n then show (round n) else show n
        where integer n = n == fromInteger (round n)
    show (Character c) = show c
    show (Quoted t) = "'" ++ show t
    show Empty = "[]"
    show (Abstraction p b) = "λ" ++ p ++ ".(" ++ show b ++ ")"
    show (Application f x) = "(" ++ show f ++ " " ++ show x ++ ")"
