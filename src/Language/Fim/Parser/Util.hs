{-# LANGUAGE FlexibleContexts #-}

module Language.Fim.Parser.Util ( space_
                                , Parser
                                , token
                                , token_
                                , tokenChoice
                                , tokenChoice_
                                , manyWithNewlines
                                ) where

import Language.Fim.Lexer.Token (Token(Newline), teq)

import Control.Applicative (many)
import Control.Monad (void)
import Data.Maybe (catMaybes)
import qualified Text.Parsec as Parsec
import Text.Parsec.Char (space)
import Text.Parsec.Combinator (choice)

type Parser = Parsec.Parsec [(Parsec.SourcePos, Token)] ()

space_ :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m ()
space_ = void space

token :: Token -> Parser Token
token x =
  Parsec.token showTok fst testTok
  where showTok = show . snd
        testTok (_, t) = if t `teq` x then Just t else Nothing

token_ :: Token -> Parser ()
token_ = void . token

tokenChoice :: [Token] -> Parser Token
tokenChoice = choice . map token

tokenChoice_ :: [Token] -> Parser ()
tokenChoice_ = void . tokenChoice

manyWithNewlines :: Parser a -> Parser [a]
manyWithNewlines p = (catMaybes <$>) . many $
  choice [ Nothing <$  token_ Newline
         , Just    <$> p
         ]
