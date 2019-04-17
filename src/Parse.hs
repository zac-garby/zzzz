module Parse
    ( Expr (..)
    , Value
    , Error
    , Result
    , parse
    , parse'
    ) where

import Text.ParserCombinators.ReadP
import Data.Char
import Data.List
import Data.Maybe
import Control.Monad

-- It might be a good idea to move these to a new module, perhaps called 'ZZZZ.Data'.
type Value = Expr
type Error = String
type Result = Either Error Value

-- This is the same data-type which is used for both parse-results and actual evaluation.
data Expr
    = Symbol String
    | Number Double
    | Str String
    | List [Expr]
    | Builtin Int ([Expr] -> Result)

instance Show Expr where
    show (Symbol x) = x
    show (Number n) = show n
    show (Str s) = "\"" ++ s ++ "\""
    show (List (Symbol "quote" : [xs@(List _)])) = "'" ++ show xs
    show (List (Symbol "quote" : xs)) = "'(" ++ intercalate " " (map show xs) ++ ")"
    show (List xs) = "(" ++ intercalate " " (map show xs) ++ ")"
    show (Builtin n _) = "<builtin. " ++ show n ++ " args>"

instance Eq Expr where
    (Symbol x) == (Symbol y) = x == y
    (Number x) == (Number y) = x == y
    (Str x) == (Str y) = x == y
    (List x) == (List y) = x == y
    _ == _ = False

space :: ReadP ()
space = void $ many (satisfy isSpace)

space1 :: ReadP ()
space1 = void $ many1 (satisfy isSpace)

letter :: ReadP Char
letter = satisfy (\x -> not $ isSpace x || x `elem` "'\"()0123456789")

digit :: ReadP Char
digit = satisfy isDigit

symbol :: ReadP String
symbol = (:) <$> letter <*> many (letter +++ digit)

escapeChar :: ReadP Char
escapeChar = do
    char '\\'
    v <- satisfy (const True)
    return $ case v of
        'n' -> '\n'
        a -> a

str :: ReadP String
str = do
    char '"'
    v <- many (escapeChar +++ satisfy (/= '"'))
    char '"'
    return v

number :: ReadP Double
number = read <$> many1 digit

sexpr :: ReadP Expr
sexpr = do
    char '('
    space
    xs <- sepBy expr space1
    space
    char ')'
    return $ List xs

quoted :: ReadP Expr
quoted = do
    char '\''
    s <- sexpr
    return $ List [Symbol "quote", s]

expr :: ReadP Expr
expr = (Symbol <$> symbol)
   +++ (Number <$> number)
   +++ (Str <$> str)
   +++ sexpr
   +++ quoted

parse :: String -> Maybe Expr
parse s = case readP_to_S expr s of
    [] -> Nothing
    xs -> (Just . fst . last) xs

parse' :: String -> Expr
parse' = fromJust . parse