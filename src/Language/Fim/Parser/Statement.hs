module Language.Fim.Parser.Statement (statement) where

import qualified Language.Fim.Types as Types
import Language.Fim.Parser.Tokens (terminator)
import Language.Fim.Parser.Value (value, variable)
import Language.Fim.Parser.Literal (literal)

import Control.Monad (void)
import Data.Maybe (isJust)
import Text.Parsec ((<?>), try)
import Text.Parsec.Text (Parser)
import Text.Parsec.Combinator (choice, optionMaybe)
import Text.Parsec.Char (string, space, newline, char)

statement :: Parser Types.Statement
statement = choice [output, declaration] <?> "statement"


output :: Parser Types.Statement
output = do
  void $ string "I "
  outputVerb
  void space
  val <- value
  terminator
  void newline
  return $ Types.Output val

outputVerb :: Parser ()
outputVerb = void $
  choice [ try $ string "said" -- sa is a prefix of sang
         , string "sang"
         , string "thought"
         , string "wrote"
         ]

declaration :: Parser Types.Statement
declaration = do
  void $ string "Did you know that "
  var <- variable
  space
  declarationVerb
  space
  isConstant <- constant <?> "always"
  val <- literal
  char '?'
  newline
  return Types.Declaration { Types.declareName = var
                           , Types.declareValue = Types.VLiteral val
                           , Types.declareIsConsnant = isConstant
                           }

declarationVerb :: Parser ()
declarationVerb = void $ choice [ string "is"
                                , string "was"
                                , try (string "has")
                                , string "had"
                                , try (string "likes")
                                , try (string "liked")
                                , string "like"
                                ]

constant :: Parser Bool
constant = do
  val <- optionMaybe $ do
    string "always"
    space
  return $ isJust val
