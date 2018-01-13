module Language.Fim.Lexer ( lexTokens
                          , pprint
                          , LexStream
                          ) where

import qualified Language.Fim.Lexer.Token as Token
import Language.Fim.Lexer.Reserved ( ReservedWords(..), toString
                                   , reservedWordList, punctuationChars)

import Control.Monad (void, when)
import Data.Functor (($>))
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Text.Parsec (ParseError, eof, parse, try, SourcePos, getPosition, (<|>))
import Text.Parsec.Text (Parser)
import Text.Parsec.Combinator ( choice, optionMaybe, many1, manyTill
                              , lookAhead, notFollowedBy)
import Text.Parsec.Char (string, char, newline, digit, anyChar, oneOf, space)


type LexStream = [(SourcePos, Token.Token)]

lexTokens :: T.Text -> Either ParseError LexStream
lexTokens = parse lexTokens' ""

lexTokens' :: Parser [(SourcePos, Token.Token)]
lexTokens' =  choice [ space >> lexTokens'
                     , comment >> lexTokens'
                     , eof $> []
                     , (:) <$> lexToken <*> lexTokens'
                     ]

comment :: Parser ()
comment = void $ choice [ try psComment
                        , parenComment -- paren isn't ambiguous, P. is
                        ]
  where
    psComment = do
      many1 $ string "P."
      string "S."
      manyTill anyChar newline
    parenComment = do
      char '('
      manyTill anyChar (char ')')

lexToken :: Parser (SourcePos, Token.Token)
lexToken = (,) <$> getPosition <*> lexToken'

-- Atomic string. Managing "try" everywhere is a pain. We only care about
-- identifiers if they're the entirety of the word. Being part of the string
-- doesn't matter.
astring :: String -> Parser String
astring str = try $ do
  strMatch <- string str
  -- don't match substrings, just the end of a token.
  lookAhead $ choice [ void space
                     , void (oneOf punctuationChars)
                     , void suffix
                     ]
  return strMatch
  where suffix = choice [ try $ string "n't"
                        , try $ string "es"
                        , string "s"
                        ]

rstring :: ReservedWords -> Parser String
rstring = astring . toString

rchoice :: [ReservedWords] -> Parser String
rchoice = choice . map rstring

-- Not followed by helper
(/>>) :: ReservedWords -> String -> Parser String
rw />> suffx = try $ do
  s <- rstring rw
  notFollowedBy $ astring suffx
  return s


lexToken' :: Parser Token.Token
lexToken' = choice
  [ rstring R_Dear $> Token.ClassStart
  , astring "Your faithful student," $> Token.ClassEnd

  -- Methods
  , rstring R_Today $> Token.MainMethod
  , rstring R_I_learned $> Token.MethodDec
  , rstring R_Thats_all_about $> Token.MethodDecEnd
  , rchoice [ R_I_would
            , R_I_remembered
            ] $> Token.Call
  , choice [ R_with />> "out"
           , rstring R_to_get
           ] $> Token.MethodReturn
  , rstring R_using $> Token.MethodArgs
  , rstring R_Then_you_get $> Token.Return

  -- Input/output
  , rchoice [ R_I_said
            , R_I_wrote
            , R_I_sang
            , R_I_thought
            ] $> Token.OutputVerb
  , rstring R_I_asked $> Token.PromptVerb
  , rchoice [ R_I_heard
            , R_I_read
            ] $> Token.InputVerb
  , rstring R_the_next $> Token.InputType

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

  , astring "Did you know that" $> Token.VariableDec
  -- Only after types, so not reserved
  , choice [ astring "s"
           , astring "es"
           ] $> Token.Plural
  , astring "many" $> Token.Many
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
  , rstring R_got_one_more $> Token.Increment
  -- subtract
  , rchoice [ R_minus
            , R_without
            ] $> Token.SubtractInfix
  , rchoice [ R_subtract
            , R_the_difference_between
            ] $> Token.SubtractPrefix
  , rstring R_got_one_less $> Token.Decrement

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

endOfVariable :: Parser ()
endOfVariable = lookAhead $ choice [ punctuation
                                   , reservedWord
                                   , arrayIndex
                                   ]
  where
    -- Array indices are trailing digits
    arrayIndex = try $ space' >> many1 digit >> endOfVariable
    -- terminals do not start with spaces
    punctuation = void $ oneOf punctuationChars
    reservedWord = void . choice . map matchWord $ reservedWordList
    -- reserved words are preceded with a space
    matchWord = try . (space' >>) . string

pprint :: LexStream -> String
pprint = unwords . map (show . snd)

space' = many1 space
