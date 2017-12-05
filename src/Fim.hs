module Fim (run, parse) where

import Fim.Types (Class)
import Fim.Parser (fimClass)

import qualified Text.Parsec as Parsec
import Text.Parsec.Combinator (many1)
import Control.Monad.Trans.Writer.Lazy

parse :: String -> Either Parsec.ParseError [Class]
parse = Parsec.parse (many1 fimClass) ""

run :: String -> Writer [String] ()
run _ = return ()
