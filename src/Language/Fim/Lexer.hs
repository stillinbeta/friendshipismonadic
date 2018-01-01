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

rchoice :: [ReservedWords] -> Parser String
rchoice = choice . map rstring

lexToken' :: Parser Token.Token
lexToken' = choice
  [ rstring R_Dear $> Token.ClassStart -- prefix of Did you Know
  , astring "Your faithful student," $> Token.ClassEnd

  , rstring R_Today $> Token.MainMethod -- prefix of That's all about
  , rstring R_learned $> Token.MethodDec -- prefix of letter
  , astring "That's all about" $> Token.MethodDecEnd

  , rchoice [ R_said
            , R_wrote
            , R_sang
            , R_thought
            ] $> Token.OutputVerb

  -- If then else
  , rchoice [ R_If
            , R_When
            ] $> Token.If
  , astring "then" $> Token.Then
  , rchoice [ R_Otherwise
            , R_Or_else
            ] $> Token.Else
  , rstring R_Thats_what_I_would_do $> Token.Fi

  -- While loops
  , rchoice [ R_Heres_what_I_did_while
            , R_As_long_as
            ] $> Token.WhileStart
  , rstring R_Thats_what_I_did $> Token.WhileEnd
  , rstring R_Heres_what_I_did $> Token.DoWhileStart
  , rstring R_I_did_this $> Token.DoWhileEnd
  , choice [ astring "while"
           , astring "as long as"
           ] $> Token.DoWhileEnd2
  , rstring R_For_every $> Token.ForStart
  , rstring R_to $> Token.To

  , Token.NumberLiteral <$> numberLiteral
  , Token.CharLiteral   <$> charLiteral
  , Token.StringLiteral <$> stringLiteral
  , rstring R_nothing $> Token.NullLiteral


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
  , rchoice [ R_was
            , R_has
            , R_had
            ] $> Token.WasHad
  , rstring R_always $> Token.VariableConstant
  , rstring R_now $> Token.Now
  , rstring R_liked $> Token.Liked
  , rchoice [ R_likes
            , R_like
           ] $> Token.Like
  , rchoice [ R_becomes
            , R_become
            ] $> Token.Become
  -- types
  , rstring R_number $> Token.NumberType
  , rchoice [ R_letter
            , R_character] $> Token.CharacterType
  , rchoice [ R_word
            , R_phrase
            , R_sentence
            , R_quote
            , R_name
            ] $> Token.StringType
  , rchoice [ R_logic
            , R_argument
            ] $> Token.BooleanType
  -- add
  , rchoice [ R_added_to
            , R_plus
            ] $> Token.AddInfix
  , rstring R_add $> Token.AddPrefix
  -- minus
  , rchoice [ R_minus
            , R_without
            ] $> Token.SubtractInfix
  , rchoice [ R_subtract
            , R_the_difference_between
            ] $> Token.SubtractPrefix
  --multiply
  , rchoice [ R_times
            , R_multiplied_with
            ] $> Token.MultiplyInfix
  , rstring R_multiply $> Token.MultiplyPrefix
  -- divide
  , rstring R_divided_by $> Token.DivideInfix
  , rstring R_divide $> Token.DividePrefix
  -- Comparisons
  -- not reserved because they always appear after a non-variable
  , astring "n't" $> Token.Nt
  , astring "not" $> Token.Not
  , astring "no" $> Token.No
  , astring "than" $> Token.Than
  , astring "less" $> Token.Less
  , choice [ astring "more"
           , astring "greater"
           ] $> Token.More
  -- Boolean
  , rstring R_either $> Token.Either
  , rstring R_or $> Token.Or
  , rstring R_its_not_the_case_that $> Token.NotTheCase

  -- Utility articles and such
  , rstring R_and $> Token.And
  , rstring R_from $> Token.From
  , rstring R_by $> Token.By
  , rstring R_I $> Token.I

  -- Never used at the start of a phrase
  , choice [ astring "the"
           , astring "an"
           , astring "a"
           ] $> Token.Article

  , rchoice [ R_yes
            , R_true
            , R_right
            , R_correct
            ] $> Token.TrueLiteral
  , rchoice [ R_false
            , R_wrong
            , R_incorrect
            ] $> Token.FalseLiteral

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
