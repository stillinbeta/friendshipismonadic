module Language.Fim (run, parse) where

import Language.Fim.Types (Class)
import Language.Fim.Parser (fimClass)

import qualified Data.Text as T
import qualified Text.Parsec as Parsec
import Text.Parsec.Combinator (many1)
import Control.Monad.Trans.Writer.Lazy

parse :: T.Text -> Either String [Class]
parse = either (Left . show) Right . Parsec.parse (many1 fimClass) ""

run :: String -> Writer [String] ()
run _ = return ()
