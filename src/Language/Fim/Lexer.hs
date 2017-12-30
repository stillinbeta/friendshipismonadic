module Language.Fim.Lexer ( lexTokens
                          , pprint
                          , LexStream
                          ) where

import qualified Language.Fim.Lexer.Token as Token
import Language.Fim.Lexer.Reserved (ReservedWords(..), toString, reservedWordList)

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

rstring :: ReservedWords -> Parser String
rstring = astring . toString

lexToken' :: Parser Token.Token
lexToken' = choice
  [ rstring R_Dear $> Token.ClassStart -- prefix of Did you Know
  , astring "Your faithful student," $> Token.ClassEnd

  , rstring R_Today $> Token.MainMethod -- prefix of That's all about
  , rstring R_I $> Token.I
  , rstring R_learned $> Token.MethodDec -- prefix of letter
  , astring "That's all about" $> Token.MethodDecEnd

  , choice [ rstring R_said
           , rstring R_wrote
           , rstring R_sang
           , rstring R_thought
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
  , rstring R_is $> Token.Is
  , rstring R_are $> Token.Are
  , choice [ rstring R_was
           , rstring R_has
           , rstring R_had
           , rstring R_liked
           ] $> Token.VariableVerb
  , rstring R_always $> Token.VariableConstant
  , rstring R_now $> Token.Now
  , choice [ rstring R_likes
           , rstring R_like
           ] $> Token.Like
  , choice [ rstring R_becomes
           , rstring R_become
           ] $> Token.Become
  , rstring R_number $> Token.NumberType
  , choice [ rstring R_letter
           , rstring R_character] $> Token.CharacterType
  , choice [ rstring R_word
           , rstring R_phrase
           , rstring R_sentence
           , rstring R_quote
           , rstring R_name
           ] $> Token.StringType

  , rstring R_and $> Token.And
  , choice [ astring "added to"
           , rstring R_plus
           ] $> Token.AddInfix
  , astring "add" $> Token.AddPrefix

  , choice [ rstring R_the
           , rstring R_an
           , rstring R_a
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
