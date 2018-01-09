module Language.Fim.Parser.Value (value
                                 , variable
                                 , methodCall) where

import qualified Language.Fim.Types as Types
import qualified Language.Fim.Lexer.Token as Token
import Language.Fim.Parser.Util (Parser, token, token_)
import Language.Fim.Parser.Literal (literal, stringLiteral, charLiteral)

import Data.Functor (($>))
import Text.Parsec.Combinator (choice, sepBy1, optionMaybe)
import Text.Parsec ((<?>), (<|>), try)

value :: Parser Types.Value
value = choice [ shallowPrefix False
               , unaryOperator
               , binaryOperatorPrefix
               ]

lazyValue :: Parser Types.Value
lazyValue = choice [ shallowPrefix True
                     , unaryOperator
                     , binaryOperatorPrefix
                     ]

-- All the types that begin with a naked value
shallowPrefix :: Bool -> Parser Types.Value
shallowPrefix lazy = do
  val <- shallowValue
  specialised val <|> general val
  where specialised val = case val of
          (Types.VVariable var) ->  methodCall var
          -- stringConcat can overlap with `pure val`
          (Types.VLiteral lit) -> try $ stringConcat lit
          _ -> fail "no specialesd parser"
        general val = if lazy
                      -- try to get the shallow value first.
                      -- This prevents the `and` in argument lists or binary operators
                      -- from matching the `and` that's a synonym for plus.
                      then choice [ pure val, binaryOperatorInfix val]
                      else choice [ binaryOperatorInfix val, pure val]

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
  expr1 <- lazyValue
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

methodCall :: Types.Variable -> Parser Types.Value
methodCall (Types.Variable idt) = do
  token_ Token.MethodArgs
  args <- sepBy1 lazyValue $ token_ Token.And
  return Types.VMethodCall { Types.vMethodName = idt
                           , Types.vMethodArgs = args
                           }

stringConcat :: Types.Literal -> Parser Types.Value
stringConcat lit
  -- Force at least one level of recursion, to avoid matching plan string
  -- literal.
  | isStrCharLit = Types.VConcat <$> (Types.CValue lit <$> value <*> stringConcat')
  | otherwise = fail "expected string or character literal"
  where
    isStrCharLit = case lit of
                     Types.StringLiteral{} -> True
                     Types.CharacterLiteral{} -> True
                     _ -> False
    stringConcat' = do
      lit' <- choice [stringLiteral, charLiteral]
      conc <- optionMaybe $ (,) <$> value <*> stringConcat'
      return $ case conc of
        Nothing -> Types.CLeaf lit'
        Just (val, subVal) -> Types.CValue lit' val subVal
