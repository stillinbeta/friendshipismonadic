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
literal = choice [const Types.Null <$> string "null" <?> "null"
                 , charLiteral <?> "character literal"
                 , numberLiteral <?> "number"
                 , stringLiteral <?> "string literal"]

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
  (match, quoteType) <- case openQuote of
        '\'' -> return ('\'' , Types.SimpleQuote)
        '‘' -> return ('’' , Types.FancyQuote)
        _ -> fail $ "unexpected matched quote " ++ [openQuote]
  char match
  return Types.CharacterLiteral { Types.clValue = value
                                , Types.clWrap = quoteType
                                }

stringLiteral :: Parser Types.Literal
stringLiteral = do
  quote <- oneOf "\"“"
  (match, quoteType) <- case quote of
        '"' -> return ('"' , Types.SimpleQuote)
        '“' -> return ('”' , Types.FancyQuote)
        _ -> fail $ "unexpected matched quote " ++ [quote]
  str <- T.pack <$> many (noneOf [match])
  void $ oneOf [match]
  return $ Types.StringLiteral str quoteType
