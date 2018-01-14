{-# LANGUAGE TemplateHaskell #-}

module Language.Fim.Lexer.Reserved ( reservedWordList
                                   , ReservedWords(..)
                                   , toString
                                   , punctuationChars
                                   ) where
import Language.Fim.Lexer.Reserved.Macro (reservedWords)
import Language.Fim.Lexer.Reserved.List (reservedWordList)

punctuationChars :: String
-- don't add ' and ‘, they confuse the parser around possesives.
punctuationChars = ",.!?‽…:\"“"

$(reservedWords reservedWordList)
