module Language.Fim.Parser.Statement (statement) where

import qualified Language.Fim.Types as Types
import Language.Fim.Parser.Tokens (terminator)
import Language.Fim.Parser.Value (value, variable)
import Language.Fim.Parser.Literal (literal)

import Control.Monad (void)
import Data.Maybe (isJust)
import Text.Parsec ((<?>), try)
import Text.Parsec.Text (Parser)
import Text.Parsec.Combinator (choice, optionMaybe, manyTill)
import Text.Parsec.Char (string, space, newline, anyChar, char)

statement :: Parser Types.Statement
statement = choice [output, declaration] <?> "statement"


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

declaration :: Parser Types.Statement
declaration = do
  void $ string "Did you know that "
  var <- variable
  space
  verb <- declarationVerb
  space
  isConstant <- isJust <$> optionMaybe (string "always" >> space)
  val <- literal
  -- will follow nill, probably won't follow others
  char '?'
  newline
  return Types.Declaration { Types.declareVerb = verb
                           , Types.declareName = var
                           , Types.declareValue = val
                           , Types.declareIsConsnant = isConstant
                           }

declarationVerb :: Parser Types.DeclareVerb
declarationVerb = choice [ string "is" >> return Types.Is
                         , string "was" >> return Types.Was
                         , try (string "has") >> return Types.Has
                         , string "had" >> return Types.Had
                         , try (string "likes") >> return Types.Likes
                         , try (string "liked") >> return Types.Liked
                         , string "like" >> return Types.Like
                         ]
