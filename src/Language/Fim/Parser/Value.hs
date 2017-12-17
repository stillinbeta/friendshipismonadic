module Language.Fim.Parser.Value (value
                                 , variable) where

import qualified Language.Fim.Types as Types
import Language.Fim.Parser.Literal (literal)
import Language.Fim.Parser.Tokens (terminator, reservedWords)

import Control.Applicative ((<|>))
import Control.Monad (void)
import qualified Data.Text as T
import Text.Parsec.Text (Parser)
import Text.Parsec ((<?>))
import Text.Parsec.Combinator (manyTill, lookAhead)
import Text.Parsec.Char (anyChar)

value :: Parser Types.Value
value =     (Types.VLiteral  <$> literal  <?> "literal")
        <|> (Types.VVariable <$> variable <?> "variable")

variable :: Parser Types.Variable
variable =
  Types.Variable . T.pack <$> manyTill anyChar endOfVariable

-- Voids needed inline to bring types into alignment
endOfVariable :: Parser ()
endOfVariable = lookAhead $ void terminator <|> void reservedWords
