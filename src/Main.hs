module Main where

import Control.Monad
import Parse
import Eval
import System.IO
import qualified Data.Map.Strict as M

prelude :: Env
prelude = Env $ M.fromList
    [ ("x",      parse' "5")
    , ("double", parse' "(lambda (x) (+ x x))")
    , ("pair", parse' "(lambda (x) '(x x))") ]

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
