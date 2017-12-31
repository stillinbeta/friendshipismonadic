module Language.Fim.Parser.Literal (literal) where

import qualified Language.Fim.Types as Types
import qualified Language.Fim.Lexer.Token as Token
import Language.Fim.Parser.Util (Parser, token, token_)

import Data.Functor (($>))
import Text.Parsec ((<?>))
import Text.Parsec.Combinator (choice)

literal :: Parser Types.Literal
literal = choice [ charLiteral    <?> "character literal"
                 , numberLiteral  <?> "number literal"
                 , stringLiteral  <?> "string literal"
                 , booleanLiteral <?> "boolean literal"
                 , nullLiteral    <?> "null literal"
                 ]

numberLiteral :: Parser Types.Literal
numberLiteral = do
  Token.NumberLiteral n <- token Token.tNumberLiteral
  return $ Types.NumberLiteral n

stringLiteral :: Parser Types.Literal
stringLiteral = do
  Token.StringLiteral t <- token Token.tStringLiteral
  return $ Types.StringLiteral t

charLiteral :: Parser Types.Literal
charLiteral = do
  Token.CharLiteral c <- token Token.tCharLiteral
  return $ Types.CharacterLiteral c

booleanLiteral :: Parser Types.Literal
booleanLiteral = choice [ token_ Token.TrueLiteral  $> Types.BooleanLiteral True
                        , choice [ token_ Token.FalseLiteral
                                 , token_ Token.No
                                 ] $> Types.BooleanLiteral False
                        ]

nullLiteral :: Parser Types.Literal
nullLiteral = token_ Token.NullLiteral $> Types.NullLiteral
