module Language.Fim (run, runIO, parse) where

import Language.Fim.Parser (parse)
import Language.Fim.Eval (runClass, runClassIO)

import qualified Data.Text as T

run :: T.Text -> T.Text -> Either T.Text T.Text
run str input = do
  cls <- parse' str
  runClass cls input
  where parse' = either (Left . T.pack) Right . parse

runIO :: T.Text -> IO (Maybe String)
runIO str = case parse str of
  Left err -> return $ Just (show err)
  Right cls -> runClassIO cls
