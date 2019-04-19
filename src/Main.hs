module Main where

import ZZZZ.Parse
import ZZZZ.Eval
import ZZZZ.Data hiding (get)
import ZZZZ.Builtin

import Control.Monad
import Control.Monad.State
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
main = do
    runStateT repl prelude
    return ()

repl :: StateT Env IO ()
repl = forever $ do
    line <- lift $ do
        putStr "zzzz> "
        hFlush stdout
        getLine
    
    case parse line of
        Nothing -> lift $ putStrLn "no parse"
        Just p -> run p

run :: Expr -> StateT Env IO ()
run p = do
    env <- get
    case evaluate env p of
        Err err -> lift $ putStrLn $ "error: " ++ err
        Ok v t -> do
            lift $ print v
            modify t