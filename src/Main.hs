module Main where

import System.IO
import Control.Monad

import ZZZZ.Compile
import ZZZZ.Eval
import ZZZZ.Data

main :: IO ()
main = void $ repl mempty

-- | Repeats REPL iterations infinitely.
repl :: Env -> IO Env
repl env = do
    env' <- rep env
    repl env'

-- | A single REPL iteration. Takes an input, evaluates it, and prints out the result.
rep :: Env -> IO Env
rep env = do
    putStr "λ> "
    hFlush stdout
    inp <- getLine
    case compileString inp >>= whnf env of
        Left err -> putStrLn $ "error: " ++ err
        Right term -> print term
    return env