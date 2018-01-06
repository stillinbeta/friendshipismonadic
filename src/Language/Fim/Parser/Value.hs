module Language.Fim.Parser.Value (value
                                 , variable
                                 , methodCall) where

import qualified Language.Fim.Types as Types
import qualified Language.Fim.Lexer.Token as Token
import Language.Fim.Parser.Util (Parser, token, token_)
import Language.Fim.Parser.Literal (literal)

import Data.Functor (($>))
import Text.Parsec.Combinator (choice, sepBy1)
import Text.Parsec ((<?>), (<|>), try)

value :: Parser Types.Value
value = choice [ shallowPrefix
               , unaryOperator
               , binaryOperatorPrefix
               ]

greedyValue :: Parser Types.Value
greedyValue = choice [ try $ shallowValue >>= methodCall
                     , shallowValue
                     , unaryOperator
                     , binaryOperatorPrefix
                     , shallowValue >>= binaryOperatorInfix
                     ]

-- All the types that begin with a naked value
shallowPrefix :: Parser Types.Value
shallowPrefix = do
  val <- shallowValue
  choice [ methodCall val
         , binaryOperatorInfix val
         , pure val
         ]

unaryOperator :: Parser Types.Value
unaryOperator = do
  opr <- choice [ choice [ token_ Token.Not
                         , token_ Token.NotTheCase
                         ] $> Types.Not
                ]
  val <- value
  return Types.VUnaryOperation { Types.vUnArg = val
                               , Types.vUnOpr = opr
                               }

identifier :: Parser Types.Identifier
identifier = do
  Token.Identifier n <- token Token.tIdentifier
  return n

variable :: Parser Types.Variable
variable = Types.Variable <$> identifier

shallowValue :: Parser Types.Value
shallowValue =  choice [ Types.VLiteral  <$> literal  <?> "literal"
                       , Types.VVariable <$> variable <?> "variable"
                       ]

binaryOperatorInfix :: Types.Value -> Parser Types.Value
binaryOperatorInfix expr1 = do
  opr <- infixOperator <?> "infix operator"
  expr2 <- value
  return Types.VBinaryOperation { Types.vBinArg1 = expr1
                                , Types.vBinArg2 = expr2
                                , Types.vBinOpr   = opr
                                }

binaryOperatorPrefix :: Parser Types.Value
binaryOperatorPrefix = do
  (opr, infx) <- prefixOperator
  expr1 <- greedyValue
  infx
  expr2 <- value
  return Types.VBinaryOperation { Types.vBinArg1 = expr1
                                , Types.vBinArg2 = expr2
                                , Types.vBinOpr   = opr
                                }

prefixOperator :: Parser (Types.BinaryOperator, Parser ())
prefixOperator =
  choice [ token_ Token.AddPrefix      $> (Types.Add, token_ Token.And)
         , token_ Token.SubtractPrefix $> (Types.Subtract, choice [ token_ Token.And
                                                                  , token_ Token.From
                                                                  ])
         , token_ Token.MultiplyPrefix $> (Types.Multiply, token_  Token.And)
         , token_ Token.DividePrefix   $> (Types.Divide, choice [ token_ Token.And
                                                                , token_ Token.By
                                                                ])
         , token_ Token.Either         $> (Types.Xor, token_ Token.Or)
         ]

infixOperator :: Parser Types.BinaryOperator
infixOperator = choice [ token_ Token.And           $> Types.And
                       , token_ Token.AddInfix      $> Types.Add
                       , token_ Token.SubtractInfix $> Types.Subtract
                       , token_ Token.MultiplyInfix $> Types.Multiply
                       , token_ Token.DivideInfix   $> Types.Divide
                       , token_ Token.Or            $> Types.Or
                       , comparisonOperators
                       ]

comparisonOperators :: Parser Types.BinaryOperator
comparisonOperators = do
  choice [ token_ Token.Is
         , token_ Token.WasHad
         ]
  -- don't consume a Not then fail
  choice [ try (neg' >> moreThan)  $> Types.LessThanOrEqual
         , try (neg' >> lessThan)  $> Types.GreaterThanOrEqual
         , neg                     $> Types.NotEqualTo
         , moreThan                $> Types.GreaterThan
         , lessThan                $> Types.LessThan
         , pure                       Types.EqualTo
         ]
  where neg = token_ Token.Not <|> token_ Token.Nt
        neg' = neg <|> token_ Token.No
        moreThan = token_ Token.More >> token_ Token.Than
        lessThan = token_ Token.Less >> token_ Token.Than

methodCall :: Types.Value -> Parser Types.Value
methodCall (Types.VVariable (Types.Variable idt)) = do
  token_ Token.MethodArgs
  args <- sepBy1 greedyValue $ token_ Token.And
  return Types.VMethodCall { Types.vMethodName = idt
                           , Types.vMethodArgs = args
                           }
methodCall x = fail "wrong argument type"
