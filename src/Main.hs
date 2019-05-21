module Main
    ( main
    ) where

import System.IO
import Control.Monad
import Control.Monad.State (runStateT)

import ZZZZ.Compile
import ZZZZ.Eval
import ZZZZ.Data
import ZZZZ.Builtin
import ZZZZ.Parse

-- | Runs the REPL with the default builtins in the environment.
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
    expr <- input
    
    case compile expr >>= \t -> runStateT (whnf t) env of
        Left err -> do
            putStrLn $ "\ESC[1;31merror: " ++ err ++ "\ESC[0m"
            return env
        Right (res, env') -> do
            putStrLn $ "\ESC[1;32m" ++ show res ++ "\ESC[0m"
            return env'

input :: IO Expr
input = do
    putStr "\ESC[1mzzzz>\ESC[0m "
    hFlush stdout
    inp <- getLine
    case parse inp of
        Just e -> return e
        Nothing -> inputAfter inp

inputAfter :: String -> IO Expr
inputAfter prev = do
    putStr "\ESC[1mzzzz|\ESC[0m "
    hFlush stdout
    inp <- getLine
    let inp' = prev ++ "\n" ++ inp
    case parse inp' of
        Just e -> return e
        Nothing -> inputAfter inp'
