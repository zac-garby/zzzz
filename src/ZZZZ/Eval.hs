module ZZZZ.Eval
    ( reduce
    , whnf
    , isNormal
    ) where

import qualified Control.Monad.State as S
import ZZZZ.Data

-- | Performs a β-reduction on a lambda term.
reduce :: Term -> S.StateT Env Result Term

-- Things where stuff needs doing
reduce (Symbol x n) = do
    env <- S.get
    case get env x of
        Just val -> return val
        Nothing -> S.lift . Left $ "undefined symbol: " ++ x

reduce (Application (Abstraction (Symbol p n) b) x) = return $ sub p n x b
reduce (Application f x)
    | isNormal f = S.lift . Left $ "invalid application. only functions can be applied, and the parameter must be a symbol. got function: " ++ show f
    | otherwise = do
        env <- S.get
        reduced <- S.lift $ whnf env f
        return $ Application reduced x

-- Things already in normal form
reduce x = return x

-- | Checks whether or not a lambda term is in β normal form.
isNormal :: Term -> Bool
isNormal (Symbol _ _) = False
isNormal (Application _ _) = False
isNormal _ = True

-- | Reduces a term to weak-head-normal-form by repeated β-reduction, with
-- | respect to the given environment.
whnf :: Env -> Term -> Result Term
whnf env t | isNormal t = return t
           | otherwise = S.evalStateT (reduce t) env >>= whnf env

-- TODO: It's pretty stupid to
-- have abstraction parameters as generic 'Term's. There should be
-- a type representing names with subscripts, maybe Name, which stores
-- a string and an integer.