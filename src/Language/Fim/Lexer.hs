module Language.Fim.Lexer ( lexTokens
                          , pprint
                          , LexStream
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


type LexStream = [(SourcePos, Token.Token)]

lexTokens :: T.Text -> Either ParseError LexStream
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

-- Atomic string. Managing "try" everywhere is a pain
astring :: String -> Parser String
astring = try . string

lexToken' :: Parser Token.Token
lexToken' = choice
  [ astring "Dear" $> Token.ClassStart -- prefix of Did you Know
  , astring "Your faithful student," $> Token.ClassEnd

  , astring "Today" $> Token.MainMethod -- prefix of That's all about
  , astring "I" $> Token.I
  , astring "learned" $> Token.MethodDec -- prefix of letter
  , astring "That's all about" $> Token.MethodDecEnd

  , choice [ astring "said"
           , astring "wrote"
           , astring "sang"
           , astring "thought"
           ] $> Token.OutputVerb

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

  , astring "Did you know that" $> Token.VariableDec
  , astring "is" $> Token.Is
  , astring "are" $> Token.Are
  , choice [ astring "was"
           , astring "has"
           , astring "had"
           , astring "liked"
           ] $> Token.VariableVerb
  , astring "always" $> Token.VariableConstant
  , astring "now" $> Token.Now
  , choice [ astring "likes"
           , astring "like"
           ] $> Token.Like
  , choice [ astring "becomes"
           , astring "become"
           ] $> Token.Become
  , astring "number" $> Token.NumberType
  , choice [ astring "letter"
           , astring "character"] $> Token.CharacterType
  , choice [ astring "word"
           , astring "phrase"
           , astring "sentence"
           , astring "quote"
           , astring "name"
           ] $> Token.StringType

  , astring "and" $> Token.And
  , choice [ astring "added to"
           , astring "plus"
           ] $> Token.AddInfix
  , astring "add" $> Token.AddPrefix

  , choice [ astring "the"
           , astring "an"
           , astring "a"
           ] $> Token.Article

  , Token.Identifier <$> identifier1
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

-- Utility

pprint :: LexStream -> String
pprint = unwords . map (show' . snd)
  where show' x = case x of
                    Token.Newline -> "\n"
                    _ -> show x
