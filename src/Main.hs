module Main where

import ZZZZ.Parse
import ZZZZ.Eval
import ZZZZ.Data
import ZZZZ.Builtin

import Control.Monad
import System.IO
import qualified Data.Map.Strict as M

prelude :: Env
prelude = Env $ M.fromList $
    [ ("double", parse' "(lambda (x) (+ x x))")
    , ("pair",   parse' "(lambda (x) '(x x))")
    , ("not",    parse' "(lambda (a) (if (= a 'true) 'false (if (= a 'false) 'true a)))")
    , ("bottom", parse' "bottom")
    , ("⊥",      parse' "bottom") ]
    ++ builtins

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
    Err err -> putStrLn $ "error: " ++ err
    Ok v _ -> print v
