module Language.Fim.Parser (parse) where

import Language.Fim.Types (Class)
import Language.Fim.Parser.Class (fimClass)
import Language.Fim.Lexer (lexTokens)

import qualified Data.Text as T
import qualified Text.Parsec as Parsec

import Debug.Trace

parse :: T.Text -> Either String Class
parse t = either (Left . show) Right $ do
  tokens <- lexTokens t
  Parsec.parse fimClass "parser" $! tokens

traceShowWith :: (Show b) => (a -> b) -> a -> a
traceShowWith f a = traceShow (f a) a
