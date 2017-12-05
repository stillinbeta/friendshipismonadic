module Fim.Parser.Tokens (punctuation
                         , identifier) where

import Control.Monad (void)
import Text.Parsec ((<?>))
import Text.Parsec.String (Parser)
import Text.Parsec.Char (anyChar, oneOf)
import Text.Parsec.Combinator (manyTill)

identifier :: Parser String
identifier = manyTill anyChar punctuation <?> "identifier"

punctuation :: Parser ()
punctuation = void (oneOf ",.!?" <?> "punctuation")
