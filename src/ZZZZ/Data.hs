module ZZZZ.Data where
    
import Data.List

type Value = Expr
type Error = String
type Result = Either Error Value

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
    | Builtin Strategy ([Expr] -> Result)

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