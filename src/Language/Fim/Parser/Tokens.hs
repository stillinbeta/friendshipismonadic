module Language.Fim.Parser.Tokens (terminator
                                  , identifier
                                  , reservedWords
                                  , reservedWordList
                                  ) where

import Language.Fim.Types

import Control.Monad (void)
import Data.Text (pack)
import Text.Parsec ((<?>), try)
import Text.Parsec.Text (Parser)
import Text.Parsec.Char (noneOf, oneOf, string, space)
import Text.Parsec.Combinator (many1, choice)

punctuation :: String
punctuation = ",.!?"

identifier :: Parser Identifier
identifier = do
  name <- pack <$> many1 (noneOf punctuation)
  term <- terminator
  return $ Identifier name term

terminator :: Parser Terminator
terminator = do
  punct <- oneOf punctuation <?> "punctuation"
  case punct of
    '.' -> return FullStop
    ',' -> return Comma
    '?' -> return QuestionMark
    '!' -> return Exclamation
    _ -> fail $ "unknown punctuation " ++ [punct]


reservedWords :: Parser ()
-- try (space >> string Dear)
reservedWords = void $ choice $ map (try . (space>>) . string) reservedWordList

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
                   ]
