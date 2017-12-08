module Language.Fim.Parser.Tokens (terminator
                                  , identifier) where

import Language.Fim.Types

import Data.Text (pack)
import Text.Parsec ((<?>))
import Text.Parsec.Text (Parser)
import Text.Parsec.Char (noneOf, oneOf)
import Text.Parsec.Combinator (many1)

punctuation :: String
punctuation = ",.!?"

identifier :: Parser Identifier
identifier = do
  name <- pack <$> many1 (noneOf punctuation)
  term <- terminator
  return $ Identifier name term

terminator :: Parser Terminator
terminator = do
  punct <- oneOf punctuation <?> "punctuation"
  case punct of
    '.' -> return FullStop
    ',' -> return Comma
    '?' -> return QuestionMark
    '!' -> return Exclamation
