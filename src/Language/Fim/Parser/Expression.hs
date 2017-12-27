module Language.Fim.Parser.Expression (expression
                                      , variable
                                      ) where

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

expression :: Parser Types.Expression
expression =  (Types.ELiteral  <$> literal  <?> "literal")
          <|> (Types.EVariable <$> variable <?> "variable")

variable :: Parser Types.Variable
variable =
  Types.Variable . T.pack <$> manyTill anyChar endOfVariable

-- Voids needed inline to bring types into alignment
endOfVariable :: Parser ()
endOfVariable = lookAhead $ void terminator <|> void reservedWords
