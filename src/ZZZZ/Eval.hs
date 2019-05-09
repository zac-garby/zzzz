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

-- TODO: Application doesn't reduce properly. For example,
--     ((λx.(λy.x+y)) 1) 2
-- doesn't reduce (i.e. an error) because the top-level term is an application
-- of ((λx.(λy.x+y)) 1) to the number 2. However, application currently expects the
-- function to be applied to be in WHNF already (i.e. it is a λ-abstraction), but
-- this won't be the case in almost all situations.
--
-- I'm not sure how this is usually solved, but one solution might be to β-reduce the
-- function in an application to WHNF before reducing the application. Would this
-- preserve lazyness?