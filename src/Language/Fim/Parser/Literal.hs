module Language.Fim.Parser.Literal (literal) where

import qualified Language.Fim.Types as Types

import qualified Data.Text as T
import Control.Applicative (many)
import Control.Monad (void)
import Text.Parsec.Text (Parser)
import Text.Parsec.Char (oneOf, noneOf, string)
import Text.Parsec.Combinator (choice)

literal :: Parser Types.Literal
literal = choice [const Types.Null <$> string "null", stringLiteral]



stringLiteral :: Parser Types.Literal
stringLiteral = do
  quote <- oneOf "\"“"
  let (match, quoteType) = case quote of
        '"' -> ('"' , Types.SimpleQuote)
        '“' -> ('”' , Types.FancyQuote)
  str <- T.pack <$> (many $ noneOf [match])
  void $ oneOf [match]
  return $ Types.StringLiteral str quoteType
