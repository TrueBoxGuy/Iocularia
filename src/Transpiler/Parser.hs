{-|
Module      : Transpiler.Parser
Description : Parses Iocularia code.
License     : Apache License 2.0
Stability   : experimental

Parses Iocularia code.
-}

module Transpiler.Parser where

import Control.Monad.Except (ExceptT(..))
import Text.Megaparsec
import Text.Megaparsec.Char (letterChar)
import Parsers
import Transpiler.Conversion

-- | Parses a term, which must consist of at least one @'letterChar'@. This parser is able to backtrack.
term :: Parser String
term = try (some letterChar)

-- | Parses the left hand side of an expression, consisting more than 1 space separated terms, the first of which
-- being the name of the variable.
lhs :: Parser LHS
lhs = symbol $ LHS <$> symbol term <*> many (symbol term) -- sepBy is too greedy

-- | Parses the right hand side of an expression, which can either be a term, or an application of two '@rhs'.
rhs :: Parser RHS
rhs = symbol $ choice [try $ App <$> symbol part <*> symbol part, Term <$> term]
  where
    part = Term <$> term <|> brackets rhs

-- | Parses a '@Convertible'@.
expression :: Parser Convertible
expression = Expression <$> (lhs <* sSymbol "=") <*> (rhs <* sSymbol ";")

-- | Parses a program: many '@Convertible'@s.
program :: Parser [Convertible]
program = many (symbol expression)

-- | See @'inputGenerator'@.
input :: ExceptT String IO [Convertible]
input = inputGenerator "input.il" program
