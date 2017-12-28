{-# LANGUAGE FlexibleContexts #-}

module Language.Fim.Parser.Util ( space_
                                ) where

import Control.Monad (void)
import Text.Parsec (Stream, ParsecT)
import Text.Parsec.Char (space)

space_ :: (Stream s m Char) => ParsecT s u m ()
space_ = void space
