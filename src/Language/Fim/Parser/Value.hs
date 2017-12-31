module Language.Fim.Parser.Value (value
                                 , variable) where

import qualified Language.Fim.Types as Types
import qualified Language.Fim.Lexer.Token as Token
import Language.Fim.Parser.Util (Parser, token, token_)
import Language.Fim.Parser.Literal (literal)

import Data.Functor (($>))
import Text.Parsec.Combinator (choice)
import Text.Parsec ((<?>), (<|>), try)

value :: Parser Types.Value
value = choice [ shallowPrefix
               , binaryOperatorPrefix
               ]

greedyValue :: Parser Types.Value
greedyValue = choice [ shallowValue
                     , binaryOperatorPrefix
                     , shallowValue >>= binaryOperatorInfix
                     ]

-- All the types that begin with a naked value
shallowPrefix :: Parser Types.Value
shallowPrefix = do
  val <- shallowValue
  choice [ binaryOperatorInfix val
         , pure val
         ]

variable :: Parser Types.Variable
variable = do
  Token.Identifier n <- token Token.tIdentifier
  return $ Types.Variable n

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
                                                                ]
                                          )
         ]

infixOperator :: Parser Types.BinaryOperator
infixOperator = choice [ token_ Token.And           $> Types.Add
                       , token_ Token.AddInfix      $> Types.Add
                       , token_ Token.SubtractInfix $> Types.Subtract
                       , token_ Token.MultiplyInfix $> Types.Multiply
                       , token_ Token.DivideInfix   $> Types.Divide
                       , comparisonOperators
                       ]

comparisonOperators :: Parser Types.BinaryOperator
comparisonOperators = do
  choice [ token_ Token.Is
         , token_ Token.WasHad
         ]
  -- don't consume a Not then fail
  choice [ try (neg >> moreThan)  $> Types.LessThanOrEqual
         , try (neg >> lessThan)  $> Types.GreaterThanOrEqual
         , token_ Token.Not $> Types.NotEqualTo
         , moreThan         $> Types.GreaterThan
         , lessThan         $> Types.LessThan
         , pure                Types.EqualTo
         ]
  where neg = token_ Token.Not <|> token_ Token.No
        moreThan = token_ Token.More >> token_ Token.Than
        lessThan = token_ Token.Less >> token_ Token.Than
