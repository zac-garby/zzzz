module Parse
    ( Expr (..)
    , parse
    ) where

import Text.ParserCombinators.ReadP
import Data.Char
import Data.List
import Control.Monad

data Expr
    = Symbol String
    | Number Double
    | Str String
    | List [Expr]

instance Show Expr where
    show (Symbol x) = x
    show (Number n) = show n
    show (Str s) = "\"" ++ s ++ "\""
    show (List xs) = "(" ++ intercalate " " (map show xs) ++ ")"

space :: ReadP ()
space = void $ many (satisfy isSpace)

space1 :: ReadP ()
space1 = void $ many1 (satisfy isSpace)

letter :: ReadP Char
letter = satisfy (\x -> not $ isSpace x || x `elem` "\"()0123456789")

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

expr :: ReadP Expr
expr = (Symbol <$> symbol)
   +++ (Number <$> number)
   +++ (Str <$> str)
   +++ sexpr

parse :: String -> Maybe Expr
parse s = case readP_to_S expr s of
    [] -> Nothing
    xs -> (Just . fst . last) xs