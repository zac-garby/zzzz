module Main where

import System.IO
import Control.Monad

import ZZZZ.Compile

main :: IO ()
main = repl

repl = forever rep

rep :: IO ()
rep = do
    putStr "zzzz> "
    hFlush stdout
    inp <- getLine
    case compileString inp of
        Left err -> putStrLn $ "error: " ++ err
        Right term -> print term