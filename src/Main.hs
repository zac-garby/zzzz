module Main where

import System.IO
import Control.Monad

import ZZZZ.Compile
import ZZZZ.Eval
import ZZZZ.Data
import ZZZZ.Builtin

main :: IO ()
main = void $ repl (listEnv builtins)

-- | Repeats REPL iterations infinitely.
repl :: Env -> IO Env
repl env = do
    env' <- rep env
    repl env'

-- | A single REPL iteration. Takes an input, evaluates it, and prints out the result.
rep :: Env -> IO Env
rep env = do
    putStr "\ESC[1mzzzz>\ESC[0m "
    hFlush stdout
    inp <- getLine
    case compileString inp >>= whnf env of
        Left err -> putStrLn $ "\ESC[1;31merror: " ++ err ++ "\ESC[0m"
        Right term -> putStrLn $ "\ESC[1;32m" ++ show term ++ "\ESC[0m"
    return env