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
import Text.Parsec ((<?>), (<|>), try)
import Text.Parsec.Combinator (choice, optionMaybe, optional)


statements :: Parser [Types.Statement]
statements = manyWithNewlines statement

statement :: Parser Types.Statement
statement = choice [ io
                   , declaration
                   , assignment
                   , ifThenElse
                   , while
                   , doWhile
                   , for
                   ] <?> "statement"

-- output --

io :: Parser Types.Statement
io = choice [ output
            , try prompt -- might consume PromptVerb before failing
            , input
            ]

output :: Parser Types.Statement
output = do
  token_ Token.OutputVerb
  val <- value
  terminator
  token_ Token.Newline
  return $ Types.Output val

input :: Parser Types.Statement
input = do
  token_ Token.InputVerb <|> token_ Token.PromptVerb
  var <- variable
  typ <- optionMaybe $ do
    token_ Token.InputType
    declarationType
  terminator
  token_ Token.Newline
  return Types.Input { Types.inputName = var
                     , Types.inputType = typ
                     }

prompt :: Parser Types.Statement
prompt = do
  token_ Token.PromptVerb
  var <- variable
  -- ADJUSTMENT: spec as written was ambiguous here.
  token_ Token.Colon
  val <- value
  terminator
  token_ Token.Newline
  return Types.Prompt { Types.promptVal = val
                      , Types.promptName = var
                      }

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
  typ <- declarationType
  val <- optionMaybe literal
  return (Just typ, Types.VLiteral <$> val)

declarationVariable :: Parser (Maybe Types.Type, Maybe Types.Value)
declarationVariable = do
  var <- variable
  return (Nothing, Just $ Types.VVariable var)

declarationType :: Parser Types.Type
declarationType = do
  optional $ token_ Token.Article
  choice [ token_ Token.NumberType    $> Types.TNumber
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
  else_ <- optionMaybe $ do
    token_ Token.Else
    terminator
    statements
  token_ Token.Fi
  terminator
  return Types.IfThenElse { Types.ifOnVal = val
                          , Types.ifThen = then_
                          , Types.ifElse = fromMaybe [] else_
                          }

while :: Parser Types.Statement
while = do
  token_ Token.WhileStart
  val <- value
  terminator
  token_ Token.Newline
  body <- statements
  token_ Token.WhileEnd
  terminator
  token_ Token.Newline
  return Types.While { Types.whileVal  = val
                     , Types.whileBody = body
                     }

doWhile :: Parser Types.Statement
doWhile = do
  token_ Token.DoWhileStart
  terminator
  token_ Token.Newline
  body <- statements
  token_ Token.DoWhileEnd
  token_ Token.DoWhileEnd2
  val <- value
  terminator
  token_ Token.Newline
  return Types.DoWhile { Types.doWhileBody = body
                       , Types.doWhileVal  = val
                       }

for :: Parser Types.Statement
for = do
  token_ Token.ForStart
  typ <- declarationType
  var <- variable
  token_ Token.From
  val1 <- value
  token_ Token.To
  val2 <- value
  terminator
  stmts <- statements
  token_ Token.WhileEnd
  terminator
  token_ Token.Newline
  return Types.For { Types.forVar  = var
                   , Types.forType = typ
                   , Types.forFrom = val1
                   , Types.forTo   = val2
                   , Types.forBody = stmts
                   }
