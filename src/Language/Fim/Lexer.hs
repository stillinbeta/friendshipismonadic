module Language.Fim.Lexer ( lexTokens
                          ) where


import Language.Fim.Parser.Tokens (reservedWordList)
import qualified Language.Fim.Lexer.Token as Token

import Control.Monad (void, when)
import Data.Functor (($>))
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Text.Parsec (ParseError, eof, parse, try, SourcePos, getPosition, (<|>))
import Text.Parsec.Text (Parser)
import Text.Parsec.Combinator (choice, optionMaybe, many1, manyTill, lookAhead)
import Text.Parsec.Char (string, char, newline, digit, anyChar, oneOf, space)



lexTokens :: T.Text -> Either ParseError [(SourcePos, Token.Token)]
lexTokens = parse lexTokens' ""

lexTokens' :: Parser [(SourcePos, Token.Token)]
lexTokens' =  choice [ space' >> lexTokens'
                     , eof $> []
                     , (:) <$> lexToken <*> lexTokens'
                     ]

space' :: Parser Char
space' = oneOf " \t"

lexToken :: Parser (SourcePos, Token.Token)
lexToken = (,) <$> getPosition <*> lexToken'

lexToken' :: Parser Token.Token
lexToken' = choice
  [ string "Dear" $> Token.ClassStart
  , string "Your faithful student," $> Token.ClassEnd

  , string "Today" $> Token.MainMethod
  , string "I learned" $> Token.MethodDec
  , string "That's all about" $> Token.MethodDecEnd

  , Token.NumberLiteral <$> numberLiteral
  , Token.CharLiteral   <$> charLiteral
  , Token.StringLiteral <$> stringLiteral

  , char '?' $> Token.QuestionMark
  , char '!' $> Token.ExclamationPoint
  , char '.' $> Token.FullStop
  , char ':' $> Token.Colon
  , char ',' $> Token.Comma
  , char '‽' $> Token.Interrobang
  , char '…' $> Token.Ellipsis
  , newline  $> Token.Newline

  , string "Did you know that" $> Token.VariableDec
  , variableVerb $> Token.VariableVerb
  , string "number" $> Token.NumberType
  , choice [string "letter", string "chararacter"] $> Token.CharacterType
  , choice [ string "word"
           , string "phrase"
           , string "sentence"
           , string "quote"
           , string "name"
           ] $> Token.StringType

  , string "and" $> Token.And
  , string "add" $> Token.AddPrefix

  , Token.Identifier <$> identifier1
  ]

variableVerb :: Parser String
variableVerb = choice [       string "is"
                      ,       string "was"
                      , try $ string "has"
                      ,       string "had"
                      , try $ string "like"
                      , try $ string "likes"
                      ,       string "liked"
                      ]

numberLiteral :: Parser Double
numberLiteral = do
  -- EXTENSION: Fim++ doesn't have negative numbers
  -- wrap the char in a singleton array to align types for catMaybe
  sign <- optionMaybe ((:[]) <$> char '-')
  integral <- many1 digit
  -- try because `.` could be either a decimal or a FullStop terminator
  fractional <- optionMaybe . try $ do
    dot <- char '.'
    digits <- many1 digit
    return $ dot:digits
  let numAsString = concat $ catMaybes [ sign, Just integral, fractional]
  case reads numAsString of
    [(num, "")] -> return num
    _ -> fail $ "failed to read " ++ numAsString

-- Should this be simpler?
charLiteral :: Parser Char
charLiteral = do
  openQuote <- oneOf "‘'"
  value <- anyChar
  match <- case openQuote of
        '\'' -> return '\''
        '‘' -> return '’'
        _ -> fail $ "unexpected matched quote " ++ [openQuote]
  char match
  return value

stringLiteral :: Parser T.Text
stringLiteral = do
  quote <- oneOf "\"“"
  match <- case quote of
        '"' -> return '"'
        '“' -> return '”'
        _ -> fail $ "unexpected matched quote " ++ [quote]
  T.pack <$> manyTill anyChar (char match)

identifier1 :: Parser T.Text
identifier1 = do
  idt <- identifier
  when (T.null idt) $ fail "empty identifier"
  return idt

identifier :: Parser T.Text
identifier = T.pack <$> manyTill anyChar endOfVariable

-- Voids needed inline to bring types into alignment
endOfVariable :: Parser ()
endOfVariable = lookAhead $ void punctuation <|> void reservedWords

punctuation :: Parser Char
punctuation = oneOf ".!?‽…:"

reservedWords :: Parser ()
-- try (space >> string Dear)
reservedWords = void $ choice $ map (try . (space>>) . string) reservedWordList
