module Language.Fim (run, parse) where

import Language.Fim.Types (Class)
import Language.Fim.Parser (fimClass)
import Language.Fim.Eval (runClass)

import qualified Data.Text as T
import qualified Text.Parsec as Parsec
import Text.Parsec.Combinator (many1)

parse :: T.Text -> Either String [Class]
parse = either (Left . show) Right . Parsec.parse (many1 fimClass) ""

run :: T.Text -> IO (Maybe String)
run str = case parse str of
  Left err -> return $ Just (show err)
  Right [] -> return $ Just "no class found"
  Right (cls:_) -> runClass cls >> return Nothing
