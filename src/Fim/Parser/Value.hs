module Fim.Parser.Value (value) where

import qualified Fim.Types as Types
import Fim.Parser.Literal (literal)

import Text.Parsec.String (Parser)

value :: Parser Types.Value
value = Types.VLiteral <$> literal
