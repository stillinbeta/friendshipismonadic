{-# LANGUAGE FlexibleContexts #-}

module Language.Fim.Parser.Util ( Parser
                                , token
                                , token_
                                , tokenChoice
                                , tokenChoice_
                                ) where

import Language.Fim.Lexer.Token (Token, teq)

import Control.Monad (void)
import qualified Text.Parsec as Parsec
import Text.Parsec.Combinator (choice)

type Parser = Parsec.Parsec [(Parsec.SourcePos, Token)] ()

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
