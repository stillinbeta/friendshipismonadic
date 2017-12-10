module Language.Fim.Parser (parse) where

import Language.Fim.Types (Class)
import Language.Fim.Parser.Class (fimClass)

import qualified Data.Text as T
import qualified Text.Parsec as Parsec

parse :: T.Text -> Either String Class
parse = either (Left . show) Right . Parsec.parse fimClass ""
