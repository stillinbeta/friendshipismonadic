module Language.Fim (run, parse) where

import Language.Fim.Parser (parse)
import Language.Fim.Eval (runClass)

import qualified Data.Text as T

run :: T.Text -> IO (Maybe String)
run str = case parse str of
  Left err -> return $ Just (show err)
  Right cls -> runClass cls >> return Nothing
