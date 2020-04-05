{-|
Module      : Parsers
Description : Parser helper functions
License     : Apache License 2.0
Stability   : experimental

Contains helpers for parsers and for running parsers.
-}

module Parsers where

import Control.Monad.Combinators (between, sepBy)
import Control.Monad.Except (ExceptT(..))
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (space, string)
import Paths_Lambda_Calculus

-- | The default type used for a parser.
type Parser = Parsec Void String

-- | Consumes all spaces around a parser.
symbol :: Parser a -> Parser a
symbol = (<* space) . (space *>)

-- | Consumes a string, whilst remaining ignorant to spaces around it.
sSymbol :: String -> Parser String
sSymbol = symbol . string

-- | Wraps a parser in brackets that ignore whitespace.
brackets :: Parser a -> Parser a
brackets =  between (sSymbol "(") (sSymbol ")")

-- | Creates an '@ExceptT@ @String@ @IO@ a', that reads and parses a file, whilst displaying errors accordingly.
-- This does not handle IO errors.
inputGenerator :: String -> Parser a -> ExceptT String IO a
inputGenerator file parser = ExceptT (parseString <$> (getDataFileName file >>= readFile))
  where parseString = either (Left . errorBundlePretty) Right . runParser (parser <* eof) ""
