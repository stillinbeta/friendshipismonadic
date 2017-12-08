module Language.Fim.Parser.Value (value) where

import qualified Language.Fim.Types as Types
import Language.Fim.Parser.Literal (literal)
import Language.Fim.Parser.Tokens (terminator)

import Control.Applicative ((<|>))
import qualified Data.Text as T
import Text.Parsec.Text (Parser)
import Text.Parsec ((<?>))
import Text.Parsec.Combinator (manyTill, lookAhead)
import Text.Parsec.Char (anyChar)

value :: Parser Types.Value
value = Types.VLiteral <$> literal
        <|> Types.VVariable <$> variable <?> "variable"

variable :: Parser Types.Variable
variable = Types.Variable . T.pack <$> manyTill anyChar (lookAhead terminator)
