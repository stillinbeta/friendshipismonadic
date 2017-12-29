module Language.Fim.Parser.Tokens (terminator
                                  , identifier
                                  , reservedWordList
                                  ) where

import qualified Data.Text as T
import qualified Language.Fim.Lexer.Token as Token
import Language.Fim.Parser.Util (Parser, token_, token)

import Text.Parsec.Combinator (choice)

identifier :: Parser T.Text
identifier = do
  Token.Identifier name <- token Token.tIdentifier
  return  name

terminator :: Parser ()
terminator = choice [ token_ Token.FullStop
                    , token_ Token.ExclamationPoint
                    , token_ Token.QuestionMark
                    , token_ Token.Interrobang
                    , token_ Token.Ellipsis
                    , token_ Token.Colon
                    ]

reservedWordList :: [String]
reservedWordList = [ "Dear"
                   -- declare
                   , "Did"
                   , "you"
                   , "know"
                   , "that"
                   , "always"
                   -- declare verbs
                   , "is"
                   , "are"
                   , "was"
                   , "has"
                   , "had"
                   , "like"
                   , "likes"
                   , "liked"
                   -- Output verbs
                   , "sang"
                   , "wrote"
                   , "said"
                   , "thought"
                   -- types
                   , "letter"
                   , "letter"
                   , "character"
                   , "word"
                   , "phrase"
                   , "sentence"
                   , "quote"
                   , "name"
                   -- modification verbs
                   , "now"
                   , "become"
                   , "becomes"
                   -- Binary Arithmatic Operators
                   , "add"
                   , "plus"
                   , "and"
                   , "added"
                   , "to"
                   ]
