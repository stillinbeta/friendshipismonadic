module Language.Fim.Parser.Literal (literal
                                   , stringLiteral
                                   , charLiteral
                                   ) where

import qualified Language.Fim.Types as Types
import qualified Language.Fim.Lexer.Token as Token
import Language.Fim.Parser.Util (Parser, token, token_)

import Data.Functor (($>))
import Text.Parsec ((<?>), (<|>))
import Text.Parsec.Combinator (choice)

literal :: Parser Types.Literal
literal = choice [ charLiteral    <?> "character literal"
                 , numberLiteral  <?> "number literal"
                 , stringLiteral  <?> "string literal"
                 , booleanLiteral <?> "boolean literal"
                 , nullLiteral    <?> "null literal"
                 ]

numberLiteral :: Parser Types.Literal
numberLiteral = numberLit <|> intLit
  where
    numberLit =
      Types.NumberLiteral
      . Token.numberLiteral
      <$> token Token.tNumberLiteral
    intLit =
      Types.NumberLiteral
      . fromIntegral
      . Token.intLiteral
      <$> token Token.tIntLiteral

stringLiteral :: Parser Types.Literal
stringLiteral =
  Types.StringLiteral . Token.stringLiteral <$> token Token.tStringLiteral

charLiteral :: Parser Types.Literal
charLiteral =
  Types.CharacterLiteral . Token.charLiteral <$> token Token.tCharLiteral

booleanLiteral :: Parser Types.Literal
booleanLiteral = choice [ token_ Token.TrueLiteral  $> Types.BooleanLiteral True
                        , choice [ token_ Token.FalseLiteral
                                 , token_ Token.No
                                 ] $> Types.BooleanLiteral False
                        ]

nullLiteral :: Parser Types.Literal
nullLiteral = token_ Token.NullLiteral $> Types.NullLiteral
