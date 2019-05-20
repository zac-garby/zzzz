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
reduce (Symbol "⊥" _) = S.lift . Left $ "bottom value evaluated"
reduce (Symbol x n) = do
    env <- S.get
    case get env x of
        Just val -> return val
        Nothing -> S.lift . Left $ "undefined symbol: " ++ x

reduce (Application (Abstraction (Symbol p n) b) x) = return $ sub p n x b
reduce (Application (Application (Symbol "def" _) (Symbol n _)) v) = do
    S.modify (set n v)
    return v
reduce (Application (Application (Symbol "def" _) _) _) = do
    S.lift . Left $ "malformed definition. a def expression should be in the form:\n\t(def name value)\n\twhere name is a symbol"
reduce (Application (Builtin s b) x) = case s of
    Lazy -> S.lift $ b x
    Strict -> do
        x' <- whnf x
        S.lift $ b x'
reduce (Application (Application (Symbol "cons" n) a) b)
    | isNormal a = do -- If a is in normal form, then b must not be
        b' <- reduce b
        return $ Application (Application (Symbol "cons" n) a) b'
    | otherwise = do -- Otherwise, b is in normal form and a isn't.
        a' <- reduce a
        return $ Application (Application (Symbol "cons" n) a') b
reduce (Application f x)
    | isNormal f = S.lift . Left $ "attempted to apply non-function: " ++ show f ++ ".\n\tmaybe you applied a function to too many arguments?"
    | otherwise = do
        reduced <- whnf f
        return $ Application reduced x

-- Things already in normal form
reduce x = return x

-- | Checks whether or not a lambda term is in β normal form.
isNormal :: Term -> Bool
isNormal (Symbol _ _) = False
isNormal (Application (Application (Symbol "cons" _) a) b) = isNormal a && isNormal b
isNormal (Application _ _) = False
isNormal _ = True

-- | Reduces a term with respect to an environment while the predicate
-- holds true on the reduced term.
reduceWhile :: (Term -> Bool) -> Term -> S.StateT Env Result Term
reduceWhile fn t | not (fn t) = return t
                 | otherwise = reduce t >>= reduceWhile fn

-- | Reduces a term with respect to an environment until the predicate
-- evaluates to true on the reduced term.
reduceUntil :: (Term -> Bool) -> Term -> S.StateT Env Result Term
reduceUntil fn = reduceWhile (not . fn)

-- | Reduces a term to weak-head-normal-form by repeated β-reduction, with
-- respect to the state's environment. The environment may be changed during
-- evaluation.
whnf :: Term -> S.StateT Env Result Term
whnf = reduceUntil isNormal

-- TODO: It's pretty stupid to
-- have abstraction parameters as generic 'Term's. There should be
-- a type representing names with subscripts, maybe Name, which stores
-- a string and an integer.