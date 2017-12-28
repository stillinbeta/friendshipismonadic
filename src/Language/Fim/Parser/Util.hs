{-# LANGUAGE FlexibleContexts #-}

module Language.Fim.Parser.Util ( space_
                                , Parser
                                , token
                                , token_
                                ) where

import Language.Fim.Lexer.Token (Token, teq)

import Control.Monad (void)
import qualified Text.Parsec as Parsec
import Text.Parsec.Char (space)

space_ :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m ()
space_ = void space

token :: Token -> Parser Token
token x = Parsec.token showTok fst testTok
  where showTok = show . snd
        testTok (_, t) = if t `teq` x then Just t else Nothing

token_ :: Token -> Parser ()
token_ = void . token

type Parser = Parsec.Parsec [(Parsec.SourcePos, Token)] ()
