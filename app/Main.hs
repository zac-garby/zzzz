module Main where

import Control.Monad
import Parse

main :: IO ()
main = repl

repl :: IO ()
repl = forever $ do
    putStr "zzzz> "
    line <- getLine
    case parse line of
        Nothing -> putStrLn "no parse"
        Just p -> print p