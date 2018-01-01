module Language.Fim.Parser.Statement ( statement
                                     , statements
                                     ) where

import qualified Language.Fim.Types as Types
-- import Language.Fim.Parser.Tokens (terminator)
import Language.Fim.Parser.Value (value, variable)
import Language.Fim.Parser.Util (Parser, token_, tokenChoice_, manyWithNewlines)
import Language.Fim.Parser.Tokens (terminator)
import Language.Fim.Parser.Literal (literal)
import qualified Language.Fim.Lexer.Token as Token

import Data.Functor (($>))
import Data.Maybe (isJust, fromMaybe)
import Text.Parsec ((<?>), (<|>))
import Text.Parsec.Combinator (choice, optionMaybe, optional)


statements :: Parser [Types.Statement]
statements = manyWithNewlines statement

statement :: Parser Types.Statement
statement = choice [ output
                   , declaration
                   , assignment
                   , ifThenElse
                   ] <?> "statement"

-- output --
output :: Parser Types.Statement
output = do
  token_ Token.I
  token_ Token.OutputVerb
  val <- value
  terminator
  token_ Token.Newline
  return $ Types.Output val

-- Declaration --

variableVerb :: Parser ()
variableVerb = tokenChoice_ [ Token.Is
                            , Token.WasHad
                            , Token.Like
                            , Token.Liked
                            ]

declaration :: Parser Types.Statement
declaration = do
  token_ Token.VariableDec
  var <- variable
  variableVerb
  isConstant <- isJust <$> optionMaybe (token_ Token.VariableConstant)
  (typ, val) <-  choice [ declarationTyped
                        , declarationVariable
                        ]
  terminator
  token_ Token.Newline
  return Types.Declaration { Types.declareName = var
                           , Types.declareVal = val
                           , Types.declareType = typ
                           , Types.declareIsConstant = isConstant
                           }

declarationTyped :: Parser (Maybe Types.Type, Maybe Types.Value)
declarationTyped = do
  optional $ token_ Token.Article
  typ <- declarationType
  val <- optionMaybe literal
  return (Just typ, Types.VLiteral <$> val)

declarationVariable :: Parser (Maybe Types.Type, Maybe Types.Value)
declarationVariable = do
  var <- variable
  return (Nothing, Just $ Types.VVariable var)

declarationType :: Parser Types.Type
declarationType = choice [ token_ Token.NumberType    $> Types.TNumber
                         , token_ Token.CharacterType $> Types.TCharacter
                         , token_ Token.StringType    $> Types.TString
                         , token_ Token.BooleanType   $> Types.TBoolean
                         ]

-- Assignment --

assignment :: Parser Types.Statement
assignment = do
  var <- variable <?> "variable"
  assignmentVerb
  val <- value
  terminator
  token_ Token.Newline
  return Types.Assignment { Types.assignmentName = var
                          , Types.assignmentExpr = val
                          }

assignmentVerb :: Parser ()
assignmentVerb = choice [ (token_ Token.Is <|> token_ Token.Are) >> token_ Token.Now
                        , token_ Token.Now >> token_ Token.Like
                        , token_ Token.Become
                        ]

-- If then else
ifThenElse :: Parser Types.Statement
ifThenElse = do
  token_ Token.If
  val <- value
  optional $ token_ Token.Then
  terminator
  token_ Token.Newline
  then_ <- statements
  else_ <- fromMaybe [] <$> optionMaybe (do
    token_ Token.Else
    terminator
    statements)
  token_ Token.Fi
  terminator
  return Types.IfThenElse { Types.ifOnVal = val
                          , Types.ifThen = then_
                          , Types.ifElse = else_
                          }
