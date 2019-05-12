module Main where

import System.IO
import Control.Monad

import ZZZZ.Compile
import ZZZZ.Eval

main :: IO ()
main = repl

-- | Repeats REPL iterations infinitely.
repl = forever rep

-- | A single REPL iteration. Takes an input, evaluates it, and prints out the result.
rep :: IO ()
rep = do
    putStr "λ> "
    hFlush stdout
    inp <- getLine
    case compileString inp >>= whnf mempty of
        Left err -> putStrLn $ "error: " ++ err
        Right term -> print term