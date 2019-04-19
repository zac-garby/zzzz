module ZZZZ.Parse
    ( Expr (..)
    , parse
    , parse'
    ) where

import ZZZZ.Data

import Text.ParserCombinators.ReadP
import Data.Char
import Data.Maybe
import Control.Monad

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

integer :: ReadP Double
integer = read <$> many1 digit

decimal :: ReadP Double
decimal = read <$> do
    int <- many1 digit
    char '.'
    frac <- many1 digit
    return $ int ++ "." ++ frac

number :: ReadP Double
number = decimal <++ integer

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
    s <- expr
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
    xs -> case last xs of
        (expr, "") -> Just expr
        _ -> Nothing

parse' :: String -> Expr
parse' = fromJust . parse