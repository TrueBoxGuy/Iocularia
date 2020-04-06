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
import Text.Megaparsec.Char (letterChar, string)
import Parsers
import Transpiler.Conversion

-- | Parses a term, which must consist of at least one @'letterChar'@. This parser is able to backtrack.
term :: Parser String
term = try (some letterChar)

-- | Parses the left hand side of an equation, consisting more than 1 space separated terms, the first of which
-- being the name of the variable.
lhs :: Parser LHS
lhs = symbol $ LHS <$> symbol term <*> many (symbol term) -- sepBy is too greedy

-- | Parses the right hand side of an equation, which can either be a term, or an application of two '@rhs'.
rhs :: Parser RHS
rhs = symbol $ choice [try $ App <$> symbol part <*> part, Term <$> term]
  where
    part = Term <$> term <|> brackets rhs

-- | Parses an @'Equation'@.
equation :: Parser Equation
equation = Equation <$> (lhs <* sSymbol "=") <*> (rhs <* string ";")

-- | Parses a program: many @'Equation'@s.
program :: Parser [Equation]
program = many (symbol equation)

-- | See @'inputGenerator'@.
input :: ExceptT String IO [Equation]
input = inputGenerator "input.il" program
