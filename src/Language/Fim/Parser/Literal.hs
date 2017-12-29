module Language.Fim.Parser.Literal (literal) where

import qualified Language.Fim.Types as Types
import qualified Language.Fim.Lexer.Token as Token
import Language.Fim.Parser.Util (Parser, token)

import Text.Parsec ((<?>))
import Text.Parsec.Combinator (choice)

literal :: Parser Types.Literal
literal = choice [ charLiteral   <?> "character literal"
                 , numberLiteral <?> "number literal"
                 , stringLiteral <?> "string literal"
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
