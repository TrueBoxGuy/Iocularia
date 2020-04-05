{-|
Module      : Lambda.Parser
Description : Parses lambda code.
License     : Apache License 2.0
Stability   : experimental

Parses lambda code.
-}

module Lambda.Parser where

import Control.Monad.Except (ExceptT(..))
import Text.Megaparsec
import Lambda.Lambda
import Parsers
import qualified Text.Megaparsec.Char.Lexer as L

-- | Parses a lambda term, which refers to the term instantiated by the lambda at a certain depth (0-indexed).
term :: Parser (Expr Integer)
term = Term <$> symbol (L.decimal) <?> "indexed term (natural number)"

-- | Parses the application of two expressions, which must be wrapped in brackets.
application :: Parser (Expr Integer)
application = App <$> brackets expression <*> brackets expression <?> "application"

-- | Parses a lambda, which consists of a dot and an expression.
lambda :: Parser (Expr Integer)
lambda = Lambda <$> (sSymbol "." *> expression) <?> "lambda"

-- | Parses an expression, which is either a(n): @'term'@;  @'lambda'@; @'application'@.
expression :: Parser (Expr Integer)
expression = choice [term, application, lambda] <?> "lambda expression"

-- | See @'inputGenerator'@.
input :: ExceptT String IO (Expr Integer)
input = inputGenerator "input.la" (symbol expression)
