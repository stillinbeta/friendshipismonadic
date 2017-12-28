module Language.Fim.Parser.Expression (expression
                                      , variable
                                      ) where

import qualified Language.Fim.Types as Types
import Language.Fim.Parser.Literal (literal)
import Language.Fim.Parser.Tokens (terminator, reservedWords)
import Language.Fim.Parser.Util (space_)

import Control.Applicative ((<|>))
import Control.Monad (void)
import qualified Data.Text as T
import Data.Functor (($>))
import Text.Parsec.Text (Parser)
import Text.Parsec ((<?>), try)
import Text.Parsec.Combinator (manyTill, lookAhead, choice)
import Text.Parsec.Char (anyChar, string)

expression :: Parser Types.Expression
expression = choice [ try binaryOperatorPrefix
                    , try binaryOperatorInfix
                    , shallowExpression
                    ]

shallowExpression :: Parser Types.Expression
shallowExpression = (Types.ELiteral  <$> literal  <?> "literal")
                <|> (Types.EVariable <$> variable <?> "variable")

variable :: Parser Types.Variable
variable =
  Types.Variable . T.pack <$> manyTill anyChar endOfVariable

-- Voids needed inline to bring types into alignment
endOfVariable :: Parser ()
endOfVariable = lookAhead $ void terminator <|> void reservedWords

binaryOperatorInfix :: Parser Types.Expression
binaryOperatorInfix = do
  expr1 <- shallowExpression
  space_
  opr <- infixOperator <?> "infix operator"
  space_
  expr2 <- shallowExpression
  return Types.EBinaryOperator { Types.eBinArg1 = expr1
                               , Types.eBinArg2 = expr2
                               , Types.eBinOp   = opr
                               }

binaryOperatorPrefix :: Parser Types.Expression
binaryOperatorPrefix = do
  (opr, infx) <- prefixOperator
  space_
  expr1 <- shallowExpression
  space_
  infx
  space_
  expr2 <- shallowExpression
  return Types.EBinaryOperator { Types.eBinArg1 = expr1
                               , Types.eBinArg2 = expr2
                               , Types.eBinOp   = opr
                               }

prefixOperator :: Parser (Types.BinaryOperator, Parser ())
prefixOperator =
  choice [ string "add" $> (Types.Add, void $ string "and")
         ]

infixOperator :: Parser Types.BinaryOperator
infixOperator = choice [ string "and"      $> Types.Add
                       , string "plus"     $> Types.Add
                       , string "added to" $> Types.Add
                       ]
