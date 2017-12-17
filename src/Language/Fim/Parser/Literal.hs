module Language.Fim.Parser.Literal (literal) where

import qualified Language.Fim.Types as Types

import qualified Data.Text as T
import Data.Maybe (catMaybes)
import Control.Applicative (many)
import Control.Monad (void)
import Text.Parsec (try, (<?>))
import Text.Parsec.Text (Parser)
import Text.Parsec.Char (oneOf, noneOf, string, digit, char, anyChar)
import Text.Parsec.Combinator (choice, many1, optionMaybe)

literal :: Parser Types.Literal
literal = choice [ nullLiteral <?> "null"
                 , charLiteral <?> "character literal"
                 , numberLiteral <?> "number literal"
                 , stringLiteral <?> "string literal"]

nullLiteral :: Parser Types.Literal
nullLiteral = const Types.Null <$> string "null"

numberLiteral :: Parser Types.Literal
numberLiteral = do
  -- EXTENSION: Fim++ doesn't have negative numbers
  -- wrap the char in a singleton array to align types for catMaybe
  sign <- optionMaybe ((:[]) <$> char '-')
  integral <- many1 digit
  -- try because `.` could be either a decimal or a FullStop terminator
  fractional <- optionMaybe $ try $ do
    dot <- char '.'
    digits <- many1 digit
    return $ dot:digits
  let numAsString = concat $ catMaybes [ sign, Just integral, fractional]
  case reads numAsString of
    [(num, "")] -> return Types.NumberLiteral { Types.nlValue = num }
    _ -> fail $ "failed to read " ++ numAsString

charLiteral :: Parser Types.Literal
charLiteral = do
  openQuote <- oneOf "‘'"
  value <- anyChar
  match <- case openQuote of
        '\'' -> return '\''
        '‘' -> return '’'
        _ -> fail $ "unexpected matched quote " ++ [openQuote]
  char match
  return Types.CharacterLiteral { Types.clValue = value
                                }

stringLiteral :: Parser Types.Literal
stringLiteral = do
  quote <- oneOf "\"“"
  match <- case quote of
        '"' -> return '"'
        '“' -> return '”'
        _ -> fail $ "unexpected matched quote " ++ [quote]
  str <- T.pack <$> many (noneOf [match])
  char match
  return $ Types.StringLiteral str
