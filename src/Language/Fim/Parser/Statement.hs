module Language.Fim.Parser.Statement (statement) where

import qualified Language.Fim.Types as Types
import Language.Fim.Parser.Tokens (terminator)
import Language.Fim.Parser.Value (value)

import Control.Monad (void)
import Text.Parsec ((<?>), try)
import Text.Parsec.Text (Parser)
import Text.Parsec.Combinator (choice)
import Text.Parsec.Char (string, space, newline)

statement :: Parser Types.Statement
statement = choice [output] <?> "statement"


output :: Parser Types.Statement
output = do
  void $ string "I "
  verb <- outputVerb
  void space
  val <- value
  term <- terminator
  void newline
  return $ Types.Output verb val term

outputVerb :: Parser Types.OutputVerb
outputVerb = do
  res <- choice [ (try $ string "said") -- sa is a prefix of sang
                , string "sang"
                , string "thought"
                , string "wrote"] <?> "said, sang, thought, or wrote"
  return $ case res of
    "said" -> Types.Said
    "wrote" -> Types.Wrote
    "sang" -> Types.Sang
    "thought" -> Types.Thought
