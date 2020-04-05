{-|
Module      : Transpiler.Scope
Description : Scope helper functions
License     : Apache License 2.0
Stability   : experimental

Contains helpers for differentiating the behaviour conversions have on bound and unbound variables.
Furthermore, this module enables for inlined variables.
-}

module Transpiler.Scope where

import qualified Lambda.Lambda as L
import Lambda.Combinators
import Data.Bool
import Data.Foldable
import Data.List
import Data.Monoid

-- | Term that can refer to either the global scope or a bound variable.
data ScopeTerm t
  = Inner t -- ^ Inner scope, referring to the term instantiated by a lambda at a certain depth.
  | Global -- ^ The global scope, which contains all variables that aren't inlined.

-- | Adds depth to all bound variables so that an expression can be wrapped in a @'L.Lambda'@ and still be valid.
addDepth :: Integral a => a -> L.Expr (ScopeTerm a) -> L.Expr (ScopeTerm a)
addDepth d = L.termReplacer depthAdder 0
  where
    depthAdder _ Global = L.Term Global
    depthAdder _ (Inner d1) = L.Term (Inner (d1 + d))

-- | Converts an expression containing only bound variables to a scoped expression.
toScopeTerm :: L.Expr a -> L.Expr (ScopeTerm a)
toScopeTerm = L.termReplacer (pure (L.Term . Inner)) 0

-- | Converts an expression containing scoped terms to one containing only scoped variables, with the assumption
-- that the global scope is at depth 0.
toTerm :: Integral a => L.Expr (ScopeTerm a) -> L.Expr a
toTerm = L.termReplacer (pure globaliser) 0
  where
    globaliser (Inner t) = L.Term t
    globaliser Global = L.Term 0

-- | Accesses the nth term of a partition of length l, by passing in @'truthAccessor'@ and @'falseAccessor'@
-- to access binary partitions of this partition.
accessIndex
  :: Integral a
  => L.Expr (ScopeTerm a) -- ^ The partition that access is being done on.
  -> a                    -- ^ The length of this partition.
  -> a                    -- ^ The term that needs to be accessed in this partition.
  -> L.Expr (ScopeTerm a) -- ^ Expression which accesses this term.
accessIndex a 1 _ = a
accessIndex a l n = newExpression
  where
    midpoint = until (>= l) (*2) 2 `div` 2
    newExpression = bool
      (accessIndex (L.App a (toScopeTerm truthAccessor)) midpoint n)
      (accessIndex (L.App a (toScopeTerm falseAccessor)) (l - midpoint) (n - midpoint))
      (n >= midpoint)

-- | Accesses a global variable from the global scope: see @'accessIndex'@ for an in detail explanation of how this works.
makeGlobalAccessor :: Integral a => [String] -> String -> Maybe (L.Expr (ScopeTerm a))
makeGlobalAccessor terms access = accessIndex (L.Term Global) (i $ length terms) . i <$> findIndex (== access) terms
  where i = fromIntegral

-- | Accesses main, but outside of the @'yCombinator'@: this enables a program to be run.
accessMain :: Integral a => [String] -> (L.Expr (ScopeTerm a)) -> Maybe (L.Expr (ScopeTerm a))
accessMain terms a = accessIndex a (fromIntegral $ length terms) . fromIntegral <$> findIndex (== "main") terms

-- | Accesses a bound variable: as the bound term refers to a term at a higer level, the length of bound terms should not be
-- added to this accessor.
makeBoundAccessor :: Integral a => [String] -> String -> Maybe (L.Expr (ScopeTerm a))
makeBoundAccessor terms access = L.Term . Inner . fromIntegral <$> findIndex (== access) terms

-- | Accesses any variable: bound variables; global variables; inlined variables.
makeAccessor
  :: Integral a
  => [String]                                         -- ^ The list of bound variable names.
  -> [String]                                         -- ^ The list of global variable names.
  -> (String -> Either String (L.Expr (ScopeTerm a))) -- ^ A function giving inlined expressions.
  -> String                                           -- ^ The variable wanting to be accessed.
  -> Either String (L.Expr (ScopeTerm a))             -- ^ The final expression that accesses the variable.
makeAccessor bound global inliner = fmap lambdaDepth . inliner <> maybeAccessors
  where
    lambdaDepth = addDepth (fromIntegral . length $ bound)
    maybeAccessors name = maybe (Left (name <> " not found")) Right
      (asum . sequence [makeBoundAccessor bound, fmap lambdaDepth . makeGlobalAccessor global] $ name)

-- | Identifies variables that can be inlined from a list of their dependencies.
identifyInlineable
  :: [(String, [String], a)] -- ^ A list containing tuples of, in order: variable name; dependencies; expressions.
  -> [(String, a)]           -- ^ A list containing tuples of, in order: inlineable variable names; expressions.
identifyInlineable list = bool (inlineable ++ identifyInlineable rest) [] (null inlineable)
  where
    dependencies (a, b, c) = b
    inlineable = (\(a, b, c) -> (a, c)) <$> filter (getAll . inlinePredicate) list
    inlinePredicate = mconcat (fmap All <$>  [null . dependencies, (/= "main") . (\(a, b, c) -> a)])
    names = fst <$> inlineable
    rest = (\(a, b, c) -> (a, filter (flip notElem names) b, c)) <$> filter (not . null . dependencies) list
