module Main where

import ZZZZ.Parse
import ZZZZ.Eval
import ZZZZ.Data

import Control.Monad
import System.IO
import qualified Data.Map.Strict as M

prelude :: Env
prelude = Env $ M.fromList
    [ ("x",      parse' "5")
    , ("double", parse' "(lambda (x) (+ x x))")
    , ("pair",   parse' "(lambda (x) '(x x))")
    , ("+",      Builtin [Strict, Strict] add)
    , ("if",     Builtin [Strict, Lazy, Lazy] ifFn)
    , ("=",      Builtin [Strict, Strict] equal) ]

add :: [Expr] -> Result
add [Number x, Number y] = ok $ Number (x + y)
add _ = err "only numbers can be added using +"

ifFn :: [Expr] -> Result
ifFn [(List [(Symbol "quote"), Symbol "true"]), a, _] = ok a
ifFn [(List [(Symbol "quote"), Symbol "false"]), _, b] = ok b
ifFn _ = err "an if-expression should be in the form (if cond a b)"

equal :: [Expr] -> Result
equal [x, y] = ok $
    if x == y then
        List [(Symbol "quote"), Symbol "true"]
    else
        List [(Symbol "quote"), Symbol "false"]
equal _ = err "this shouldn't be reached... report it as a bug"

main :: IO ()
main = repl

repl :: IO ()
repl = forever $ do
    putStr "zzzz> "
    hFlush stdout
    line <- getLine
    case parse line of
        Nothing -> putStrLn "no parse"
        Just p -> run p

run :: Expr -> IO ()
run p = case evaluate prelude p of
    Left err -> putStrLn $ "error: " ++ err
    Right v -> print v
