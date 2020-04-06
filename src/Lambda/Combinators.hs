{-|
Module      : Lambda.Combinators
Description : Lambda Combinators
License     : Apache License 2.0
Stability   : experimental

Contains some typical lambda combinators.
-}

module Lambda.Combinators where

import Lambda.Lambda

truthAccessor, falseAccessor, yCombinator :: Integral a => Expr a
-- ^ Lambda that returns the first argument.
truthAccessor = Lambda (Lambda (Term 0))
-- ^ Lambda that returns the second argument.
falseAccessor = Lambda (Lambda (Term 1))
-- ^ Lambda that passes the function itself to a function, enabling for recursin.
yCombinator
  =
  Lambda (App
  (Lambda (App (t 1) (t 1)))
  (Lambda (App (t 0) (App (t 1) (t 1)))))
    where t = Term
