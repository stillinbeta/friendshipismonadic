module Fim.Parser.Statement (statement) where

import qualified Fim.Types as Types
import Fim.Parser.Tokens (punctuation)
import Fim.Parser.Value (value)

import Control.Monad (void)
import Text.Parsec ((<?>))
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (choice)
import Text.Parsec.Char (string, space, newline)

statement :: Parser Types.Statement
statement = choice [iSaid] <?> "statement"

iSaid :: Parser Types.Statement
iSaid = do
  void $ string "I said" >> space
  val <- value
  punctuation
  void $ newline
  return $ Types.SISaid val
