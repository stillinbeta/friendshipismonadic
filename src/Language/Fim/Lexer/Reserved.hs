{-# LANGUAGE TemplateHaskell #-}

module Language.Fim.Lexer.Reserved ( reservedWordList
                                   , ReservedWords(..)
                                   , toString
                                   ) where
import Language.Fim.Lexer.Reserved.Macro (reservedWords)
import Language.Fim.Lexer.Reserved.List (reservedWordList)

$(reservedWords reservedWordList)
