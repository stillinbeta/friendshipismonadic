module Language.Fim.Parser.Methods (method
                          , emptyLine
                          ) where

import qualified Language.Fim.Types as Types
import Language.Fim.Parser.Tokens (identifier, terminator)
import Language.Fim.Parser.Statement (statement)

import Control.Monad (void)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Text.Parsec (try, (<?>))
import Text.Parsec.Text (Parser)
import Text.Parsec.Char (string, space, newline, char)
import Text.Parsec.Combinator (many1, optionMaybe, manyTill)

emptyLine :: Parser (Maybe Types.Function)
emptyLine = many1 space >> return Nothing

method :: Parser (Maybe Types.Function)
method = do
  isMain <- isJust <$> optionMaybe (string "Today ")
  void $ string "I learned "
  name <- identifier

  void newline
  stmts <- manyTill statement (try $ methodEnd name) <?> "statement or end of function"
  return $ Just (Types.Function name isMain stmts)

methodEnd :: Types.Identifier -> Parser ()
methodEnd idt =
  let name = T.unpack $ Types.idName idt in
  void $ string "That's all about " >> string name >> char '!'
