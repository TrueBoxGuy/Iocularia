{-|
Module      : Lambda.Lambda
Description : Lambda reducer
License     : Apache License 2.0
Stability   : experimental

Attempts to simplify lambda expressions.
-}

module Lambda.Lambda where

-- | A lambda expression, which can either consist of a lambda, a term, or an application.
data Expr t = Lambda (Expr t) | App (Expr t) (Expr t) | Term t deriving (Eq)

instance Show t => Show (Expr t) where
  show (Term t) = show t
  show (Lambda t) = "." ++ show t
  show (App exl exr) = mconcat ["(", show exl, ")", "(", show exr, ")"]

-- | Replaces the terms in a lamdba expression, using a given function and initial depth.
termReplacer
  :: Integral a
  => (a -> t -> Expr s) -- ^ A function that takes, the term depth and term in ordr, and generates an expression.
  -> a                  -- ^ The initial depth to use (used as a recursive parameter in code).
  -> Expr t             -- ^ The initial expression.
  -> Expr s             -- ^ The converted expression.
termReplacer f d (Term t) = f d t
termReplacer f d (Lambda t) = Lambda (termReplacer f (d + 1) t)
termReplacer f d (App exl exr) = App (termReplacer f d exl) (termReplacer f d exr)

-- | Simplifies a lambda expression, using two counters, meaning that inner expresions are only evaluated if
-- outer expressions can't be evaluated.
reduce
  :: Integral t
  => t -- ^ The lamba depth of inner simplifications being done, which dictates which term should be replaced.
  -> t -- ^ How many applications to reduce whilst doing an inner simplification: negative numbers remove the limit.
  -> Expr t -- ^ The initial expression.
  -> Expr t -- ^ The simplified expression.
reduce d 0 t = t
reduce d r (App (Lambda ex1) ex2) = reduce d (r - 1) (applicationReplacer ex1)
  where
    addDepthReplacer d2 = termReplacer (pure (depthFunction d2)) 0
    depthFunction _ t | t < d = Term t
    depthFunction a t = Term (t + a) -- all variables instantiated within the lambda: only +a is needed as a layer is lost
    applicationReplacer = termReplacer replacerFunction 0
    replacerFunction d2 t | t == d = addDepthReplacer d2 ex2
    replacerFunction d2 t | t < d = Term t
    replacerFunction d2 t = Term (t - 1)
reduce d r t@(App ex1 ex2)
  | ex1r /= ex1 = reduce d r (App ex1r ex2)
  | ex2r /= ex2 = reduce d r (App ex1 ex2r)
  | otherwise = t
  where
    ex1r = reduce d 1 ex1 -- ceases infinite expansion of fixed point combinators
    ex2r = reduce d 1 ex2
reduce d r (Lambda t) = Lambda (reduce (d + 1) (-1) t)
reduce _ _ t = t
