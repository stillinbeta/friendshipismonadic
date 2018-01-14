module Language.Fim.Parser.Statement ( statement
                                     , statements
                                     , declarationType
                                     ) where

import qualified Language.Fim.Types as Types
-- import Language.Fim.Parser.Tokens (terminator)
import Language.Fim.Parser.Value (value, lazyValue, variable, methodCall)
import Language.Fim.Parser.Util (Parser, token_, tokenChoice_)
import Language.Fim.Parser.Tokens (terminator)
import Language.Fim.Parser.Literal (literal)
import qualified Language.Fim.Lexer.Token as Token

import Control.Applicative (many)
import Data.Functor (($>))
import Data.Maybe (isJust, fromMaybe)
import Text.Parsec ((<?>), (<|>), try)
import Text.Parsec.Combinator (choice, optionMaybe, optional, sepBy1)


statements :: Parser [Types.Statement]
statements = many statement

statement :: Parser Types.Statement
statement = choice [ io
                   , declaration
                   , try assignment -- incrDecr also starts with a variable
                   , ifThenElse
                   , switch
                   , while
                   , doWhile
                   , for
                   , call
                   , return_
                   , incrDecr
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
  return $ Types.Output val

input :: Parser Types.Statement
input = do
  token_ Token.InputVerb <|> token_ Token.PromptVerb
  var <- variable
  typ <- optionMaybe $ do
    token_ Token.InputType
    declarationType
  terminator
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
  try (declarationArray var) <|> declarationScalar var

declarationScalar :: Types.Variable -> Parser Types.Statement
declarationScalar var = do
  isConstant <- isJust <$> optionMaybe (token_ Token.VariableConstant)
  (typ, val) <-  choice [ declarationTyped
                        , declarationVariable
                        ]
  terminator
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
  var <-  variable
  -- Try to turn it into a method call if possible
  val' <- methodCall var <|> pure (Types.VVariable var)
  return (Nothing, Just val')

declarationType :: Parser Types.Type
declarationType = do
  optional $ token_ Token.Article
  typ <- choice [ token_ Token.NumberType    $> Types.TNumber
                , token_ Token.CharacterType $> Types.TCharacter
                , token_ Token.StringType    $> Types.TString
                , token_ Token.BooleanType   $> Types.TBoolean
                ]
  isPlural <- isJust <$> optionMaybe (token_ Token.Plural)
  return $ if isPlural
           then Types.TArray typ
           else typ

declarationArray :: Types.Variable -> Parser Types.Statement
declarationArray var = arrayList <|> arrayNumbered
  where
    arrayNumbered = do
      token_ Token.Many
      typ <- declarationType
      assertArrayType typ
      terminator
      vals <- arrayNumberedItem 1
      return Types.ArrayDeclaration { Types.aDecName = var
                                    , Types.aDecType = typ
                                    , Types.aDecVals = vals
                                    }
    arrayNumberedItem i = do
      token_ $ Token.Identifier (Types.vName var)
      -- TODO: the interpreter should probably handle making sure the sequence
      -- is ascending but right now this information isn't retained into the
      -- evaluation phase
      token_ $ Token.NumberLiteral i
      variableVerb
      val <- value
      terminator
      next <- optionMaybe $ try (arrayNumberedItem (i+1))
      return $ val:fromMaybe [] next
    arrayList = do
      typ <- declarationType
      assertArrayType typ
      vals <- sepBy1 (lazyValue $ token_ Token.And) (token_ Token.And)
      terminator
      return Types.ArrayDeclaration { Types.aDecName = var
                                    , Types.aDecType = typ
                                    , Types.aDecVals = vals
                                    }
    assertArrayType typ =
      case typ of
        Types.TArray {} -> pure ()
        _ -> fail "Expected array type"


-- Assignment --

assignment :: Parser Types.Statement
assignment = do
  var <- variable <?> "variable"
  assignmentVerb
  val <- value
  terminator
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

-- Switch
switch :: Parser Types.Statement
switch = do
  token_ Token.SwitchStart
  val <- value
  terminator
  cases <- many case_
  def <- default_ <|> pure []
  token_ Token.WhileEnd
  terminator
  return Types.Switch { Types.switchOnVal = val
                      , Types.switchCases = cases
                      , Types.switchDefault = def
                      }
  where case_ = do
          token_ Token.SwitchCasePrefix
          lit <- literal
          optional $ token_ Token.SwitchCaseInfix
          token_ Token.SwitchCaseSuffix
          terminator
          stmts <- statements
          return Types.Case { Types.caseLit  = lit
                            , Types.caseBody = stmts
                            }
        default_ = do
          token_ Token.SwitchDefault
          terminator
          statements

-- loops
while :: Parser Types.Statement
while = do
  token_ Token.WhileStart
  val <- value
  terminator
  body <- statements
  token_ Token.WhileEnd
  terminator
  return Types.While { Types.whileVal  = val
                     , Types.whileBody = body
                     }

doWhile :: Parser Types.Statement
doWhile = do
  token_ Token.DoWhileStart
  terminator
  body <- statements
  token_ Token.DoWhileEnd
  token_ Token.DoWhileEnd2
  val <- value
  terminator
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
  return Types.For { Types.forVar  = var
                   , Types.forType = typ
                   , Types.forFrom = val1
                   , Types.forTo   = val2
                   , Types.forBody = stmts
                   }

-- calling
call :: Parser Types.Statement
call = do
  token_ Token.Call
  val <- value
  terminator
  return Types.Call { Types.callVal = val }

-- returning
return_ :: Parser Types.Statement
return_ = do
  token_ Token.Return
  val <- value
  terminator
  return Types.Return { Types.returnVal = val }

-- increment / decrement

incrDecr :: Parser Types.Statement
incrDecr = do
  var <- variable
  constructor <- choice [ token_ Token.Increment $> Types.Increment
                        , token_ Token.Decrement $> Types.Decrement
                        ]
  terminator
  return $ constructor var
