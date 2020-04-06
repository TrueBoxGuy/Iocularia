{-|
Module      : Transpiler.Conversion
Description : Converts between the Equation type and the Lambda type.
License     : Apache License 2.0
Stability   : experimental

Converts from @'Equation'@ to @'Lambda'@ using a method that: allows for recursion between multiple variables;
is suitably efficient.
-}

module Transpiler.Conversion where

import qualified Lambda.Lambda as L
import Lambda.Combinators
import Transpiler.Scope
import Control.Applicative
import Data.List
import Data.Either
import Data.Function
import Data.Bool

-- | Type representing return value of @'makeAccessor'@.
type Scope a = (String -> Either String (L.Expr (ScopeTerm a)))

-- | Represents the right hand side of an equation, which can either be a term or an application of multiple RHSses.
data RHS = App RHS RHS | Term String

-- | Represents the left hand side of an equation: an equation's name and the list of bound variables it takes.
data LHS = LHS String [String]

-- | Represents an equation that can be converted to a @'L.Expr' a@, which consists of an @'LHS'@ and @'RHS'@.
data Equation = Equation LHS RHS

-- | Gets the name of an equation
name :: Equation -> String
name (Equation (LHS n _) _) = n

-- | Converts the right hand side of an equation to a @'L.Expr' a@, given its scope.
convertRHS :: Integral a => Scope a -> RHS -> Either String (L.Expr (ScopeTerm a))
convertRHS f (Term s) = f s
convertRHS f (App l r) = L.App <$> convertRHS f l <*> convertRHS f r

-- | Generates the scope of the right hand side of an equation, and then converts it.
convertExpression :: Integral a => [String] -> Scope a -> Equation -> Either String (L.Expr (ScopeTerm a))
convertExpression global inliner (Equation (LHS lName vars) rhs)
  = addName (lambdas <$> convertRHS scope rhs)
  where
    lambdas = foldr (.) id (L.Lambda <$ vars)
    addName = either (Left . (("In definition of: " <> lName <> ", ") <>)) Right
    scope = makeAccessor vars global inliner

-- | Makes a function from which variables can be chosen, by applying @'truthAccessor'@ and @'falseAccessor'@
-- to a binary structure.
boolChoice :: Integral a => [L.Expr (ScopeTerm a)] -> L.Expr (ScopeTerm a)
boolChoice [] = L.Term $ Inner 0 -- enables for error of main not being defined
boolChoice [e] = e
boolChoice a = L.Lambda (L.App (L.App (L.Term $ Inner 0) (d $ boolChoice l)) (d $ boolChoice r))
  where
    mid = until (>= length a) (*2) 2 `div` 2
    (l, r) = splitAt mid a
    d = addDepth 1

-- | Errors with the name of a duplicate variable if it exists.
ensureNoDuplicates :: [Equation] -> Either String [String]
ensureNoDuplicates = sequence . fmap toUnique . group . sort . fmap name
  where
    name (Equation (LHS n _) _) = n
    toUnique [] = Left "the world is broken"
    toUnique [a] = Right a
    toUnique (a:_) = Left ("duplicate definitions of " <> a <> " exist")

-- | Converts equations so that @'identifyInlineable'@ can be used, and returns inlineable and global variables
-- in order.
splitInlineable :: [Equation] -> ([Equation], [Equation])
splitInlineable exprs = partition (flip elem inlineNames . name) exprs
  where
    dependencies bound (Term a) = filter (flip notElem bound) [a]
    dependencies bound (App l r) = ($ r) <> ($ l) $ dependencies bound
    list = fmap toIdentify exprs
    toIdentify t@(Equation (LHS n bound) r) = (n, dependencies bound r, t)
    inlineNames = fst <$> identifyInlineable list

-- | Converts a program into a lambda expression.
toExpression :: Integral a => [Equation] -> Either String (L.Expr a)
toExpression defs
  -- both the function and the value that the scope is being applied to must be taken in
  -- access only requires applications, so it does not cause a depth change
  = fmap toTerm . getMain . constructBinaryStructure
  =<< sequence (fmap convert globals)
  <* ensureNoDuplicates defs
    where
      (inlined, globals) = splitInlineable defs
      globalNames = name <$> globals
      inliner = flip maybe convert . Left . (<> " not found") <*>  flip find inlined . (. name) . (==)
        -- I'm sorry: I just want a small bit of my legacy to be here
      convert = convertExpression globalNames inliner
      constructBinaryStructure = L.App (toScopeTerm yCombinator) . L.Lambda . addDepth 1 . boolChoice
      getMain = maybe (Left "main isn't defined") Right . accessMain globalNames
