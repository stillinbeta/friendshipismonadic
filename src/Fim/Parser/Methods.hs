module Fim.Parser.Methods (method
                          , emptyLine
                          ) where

import qualified Fim.Types as Types
import Fim.Parser.Tokens (identifier, punctuation)
import Fim.Parser.Statement (statement)

import Control.Monad (void)
import Data.Maybe (isJust)
import Text.Parsec (try, (<?>))
import Text.Parsec.String (Parser)
import Text.Parsec.Char (string, space, newline)
import Text.Parsec.Combinator (many1, optionMaybe, manyTill)

emptyLine :: Parser (Maybe Types.Function)
emptyLine = many1 space >> return Nothing

method :: Parser (Maybe Types.Function)
method = do
  isMain <- isJust <$> optionMaybe (string "Today ")
  void $ string "I learned "
  name <- identifier
  void newline
  stmts <- manyTill statement (try $ methodEnd name)
  return $ Just (Types.Function name isMain stmts)

methodEnd :: String -> Parser ()
methodEnd name = (string "That's all about " >> string name >> punctuation) <?> "method end"
