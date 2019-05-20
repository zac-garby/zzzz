module ZZZZ.Data where

import Data.List
import Control.Monad
import Control.Monad.State hiding (get)
import qualified Control.Monad.State as S
import qualified Data.Map.Strict as M

-- | Can be returned when evaluating a value. It will be used to indicate to the user
-- which error happened.
type Error = String

-- | A Result contains either a success value or an error.
type Result = Either Error

-- | A mapping from names to values, for use in an environment.
type SymbolTable = M.Map String Term

infixr 0 |||

-- | @x ||| err@ will return an error result with the given error message if @x@ is @Nothing@,
-- and otherwise will return the value inside @x@, wrapped in @Ok@.
(|||) :: Maybe a -> Error -> Result a
Nothing ||| e = Left e
Just val ||| _ = return val

-- | Short for "environment", an @Env@ maps names to values to be retrieved when evaluating a symbol.
newtype Env = Env SymbolTable

instance Semigroup Env where
    (Env a) <> (Env b) = Env (a <> b)

instance Monoid Env where
    mempty = Env mempty

-- | Constructs a new environment from an associative list.
listEnv :: [(String, Term)] -> Env
listEnv = Env . M.fromList

-- | Returns the corresponding value to the given name inside an environment.
get :: Env -> String -> Maybe Term
get (Env st) name = M.lookup name st

-- | Sets the value of the given name to some value, in an environment.
set :: String -> Term -> Env -> Env
set name val (Env st) = Env $ M.insert name val st

-- | Specifies the evaluation strategy of a particular parameter of a builtin function.
-- A strategy is used when an argument to a builtin is evaluated -- the argument will
-- be reduced until the strategy predicate evaluates to true. In this sense, the strategy
-- specifies some criteria which an argument must meet before being passed to it.
type Strategy = Term -> Bool

-- | The lazy strategy always returns True, reflecting the fact that no reductions
-- should be made for a lazy argument.
lazy :: Strategy
lazy = const True

-- | The strict strategy always returns False, reflecting the fact that reductions should
-- be made up to the point where they can't be any more (recall that reduceUntil' will not
-- reduce any further once a stationary point has been reached.)
strict :: Strategy
strict = const False

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
        where trim xs | length xs > 32 = take 27 xs ++ " ... " ++ takeWhile (==')') (reverse xs)
                      | otherwise = xs
    show (ExList (ExSym "quote" : xs)) = "'" ++ unwords (map show xs)
    show (ExList xs) = "(" ++ unwords (map show xs) ++ ")"
    show (ExArr xs) = "[" ++ unwords (map show xs) ++ "]"

instance Eq Expr where
    (ExSym x) == (ExSym y) = x == y
    (ExNum x) == (ExNum y) = x == y
    (ExStr x) == (ExStr y) = x == y
    (ExChar x) == (ExChar y) = x == y
    (ExList x) == (ExList y) = x == y
    (ExArr x) == (ExArr y) = x == y
    _ == _ = False

-- | Represents a type of a @Term@.
data DataType
    = TAny
    | TSymbol
    | TNumber
    | TCharacter
    | TQuoted
    | TEmpty
    | TAbstraction
    | TApplication
    | TBuiltin
    deriving Eq

instance Show DataType where
    show TSymbol = "symbol"
    show TNumber = "number"
    show TCharacter = "character"
    show TQuoted = "quoted"
    show TEmpty = "empty"
    show TAbstraction = "abstraction"
    show TApplication = "application"
    show TBuiltin = "builtin"
    
typeOf :: Term -> DataType
typeOf (Symbol _ _) = TSymbol
typeOf (Number _) = TNumber
typeOf (Character _) = TCharacter
typeOf (Quoted _) = TQuoted
typeOf Empty = TEmpty
typeOf (Abstraction _ _) = TAbstraction
typeOf (Application _ _) = TApplication
typeOf (Builtin _ _) = TBuiltin

is :: Term -> DataType -> Bool
is t TAny = True
is t dt = typeOf t == dt

-- | @Term@s are the data type which is evaluated by the interpreter. They are more
-- akin to lambda calculus than lisp s-expressions in their structure. They may be
-- evaluated fully, e.g. a number or a string, or they may be unevaluated, like
-- a lambda application.
data Term
    = Symbol String Int -- ^ Usually a variable name when not quoted
    | Number Double -- ^ Holds a floating-point number
    | Character Char -- ^ A single character value
    | Quoted Term -- ^ A quoted term, signifying that it shouldn't be evaluated
    | Empty -- ^ An empty array for the end of cons-lists
    | Abstraction Term Term -- ^ A lambda abstraction, i.e. a function
    | Application Term Term -- ^ A lambda application, i.e. a function call
    | Builtin Strategy (Term -> Result Term) -- ^ A builtin function implemented in Haskell

instance Show Term where
    show (Symbol x 1) = x
    show (Symbol x n) = x ++ "<" ++ show n ++ ">"
    show (Number n)
        | isInfinite n && n < 0 = "-Inf"
        | isInfinite n && n > 0 = "Inf"
        | isInteger n = show (round n)
        | otherwise = show n
        where isInteger n = n == fromInteger (round n)
    show (Character c) = show c
    show (Quoted t) = "'" ++ show t
    show Empty = "[]"
    show (Abstraction p b) = "λ" ++ show p ++ "." ++ show b
    show a@(Application f x) = case unlist a of
        Just xs -> "[" ++ unwords (map show xs) ++ "]"
        Nothing -> "(" ++ show f ++ " " ++ show x ++ ")"
    show (Builtin _ _) = "<builtin>"

-- | The Eq instance for terms assumes that they are in weak-head-normal-form.
-- In this way, two symbols will be equal if they have the same name, even though
-- this would be unlikely to happen naturally. What I mean is that basically symbols
-- and other terms won't be evaluated to check for equality.
instance Eq Term where
    Symbol x n == Symbol x' n' = x == x' && n == n'
    Number a == Number b = a == b
    Character a == Character b = a == b
    Quoted a == Quoted b = a == b
    Empty == Empty = True
    Abstraction a b == Abstraction c d = a == c && b == d
    Application a b == Application c d = a == c && b == d
    _ == _ = False

-- | Utilises currying to apply a lambda abstraction to multiple arguments.
apply :: Term -> [Term] -> Term
apply = foldl' Application

-- | Constructs a cons-list by repeatedly applying the cons function.
mkList :: [Term] -> Term
mkList [] = Empty
mkList (x:xs) = apply (Symbol "cons" 1) [x, mkList xs]

-- | Converts a cons-list into a list of terms. Will return Nothing if the
-- input isn't a cons-list.
unlist :: Term -> Maybe [Term]
unlist (Application (Application (Symbol "cons" _) x) xs) = (x :) <$> unlist xs
unlist Empty = Just []
unlist _ = Nothing

-- | Substitutes a value in place of a symbol inside a symbol.
sub :: String -> Int -> Term -> Term -> Term
sub sym num to (Symbol s n)
    | s == sym && n == num = to
    | otherwise = Symbol s n
sub sym num to (Abstraction p b) = Abstraction p (sub sym num to b)
sub sym num to (Application f x) = Application (sub sym num to f) (sub sym num to x)
sub _ _ _ x = x

-- | Checks whether or not a lambda term is in β normal form.
isNormal :: Term -> Bool
isNormal (Symbol _ _) = False
isNormal (Application (Application (Symbol "cons" _) a) b) = isNormal a && isNormal b
isNormal (Application _ _) = False
isNormal _ = True

-- | Assigns indices to each symbol in a term, in order to evaluate it without
-- ambiguity. An error can arise if an abstraction's parameter isn't a symbol.
renumber :: Term -> Term
renumber t = evalState (renumber' t) 1
    where renumber' :: Term -> State Int Term
          renumber' (Abstraction (Symbol p n) b) = do
              body <- renumber' b
              newN <- increment
              let body' = sub p n (Symbol p newN) body
              return $ Abstraction (Symbol p newN) body'
          
          renumber' (Abstraction _ _) = error "pattern matching doesn't work yet"
          
          renumber' (Application f x) = do
              f' <- renumber' f
              x' <- renumber' x
              return $ Application f' x'
          
          renumber' x = return x
          
          increment = do
              n <- S.get
              put (n + 1)
              return n