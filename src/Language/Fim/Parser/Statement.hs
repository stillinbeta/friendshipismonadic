module Language.Fim.Parser.Statement (statement) where

import qualified Language.Fim.Types as Types
import Language.Fim.Parser.Tokens (terminator)
import Language.Fim.Parser.Value (value, variable)
import Language.Fim.Parser.Literal (literal)

import Control.Monad (void)
import Data.Maybe (isJust)
import Text.Parsec ((<?>), try)
import Text.Parsec.Text (Parser)
import Text.Parsec.Combinator (choice, optionMaybe, optional)
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
  isConstant <- constant
  (typ, val) <- choice [ declarationTyped
                       , declarationVariable]
  char '?'
  newline
  return Types.Declaration { Types.declareName = var
                           , Types.declareValue = val
                           , Types.declareIsConsnant = isConstant
                           , Types.declareType = typ
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

declarationTyped :: Parser (Maybe Types.Type, Types.Value)
declarationTyped = do
  declarationOptionalArticle
  typ <- declarationType
  val <- optionMaybe $ space >> literal
  return (Just typ, maybe Types.VNull Types.VLiteral val)

declarationVariable :: Parser (Maybe Types.Type, Types.Value)
declarationVariable = do
  var <- variable
  return (Nothing, Types.VVariable var)

declarationOptionalArticle :: Parser ()
declarationOptionalArticle = optional $ choice [ try $ string "the "
                                               , try $ string "a "]

declarationType :: Parser Types.Type
declarationType = choice [ try (string "number")    >> pure Types.TNumber

                         , try (string "letter")    >> pure Types.TCharacter
                         , try (string "character") >> pure Types.TCharacter

                         , try (string "word")      >> pure Types.TString
                         , try (string "phrase")    >> pure Types.TString
                         , try (string "sentence")  >> pure Types.TString
                         , try (string "quote")     >> pure Types.TString
                         , try (string "name")      >> pure Types.TString
                         ]

constant :: Parser Bool
constant = do
  val <- optionMaybe $ do
    try $ string "always"
    space
  return $ isJust val
