module Language.Fim.Parser.Expression (
                                      ) where

import qualified Language.Fim.Types as Types
import Language.Fim.Parser.Literal (literal)
import Language.Fim.Parser.Value (variable)
import qualified Language.Fim.Lexer.Token as Token
import Language.Fim.Parser.Util (Parser, token_)

import Data.Functor (($>))
import Text.Parsec ((<?>), try)
import Text.Parsec.Combinator (choice)
