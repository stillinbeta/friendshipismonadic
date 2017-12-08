module Language.Fim.Parser.Value (value) where

import qualified Language.Fim.Types as Types
import Language.Fim.Parser.Literal (literal)

import Text.Parsec.Text (Parser)

value :: Parser Types.Value
value = Types.VLiteral <$> literal
