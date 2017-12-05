module Fim.Parser.Literal (literal) where

import qualified Fim.Types as Types

import Control.Applicative (many)
import Control.Monad (void)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (oneOf, noneOf)

literal :: Parser Types.Literal
literal = do
  void $ oneOf "\"“"
  str <- many $ noneOf "\"”"
  void $ oneOf "\"”"
  return $ Types.StringLiteral str
