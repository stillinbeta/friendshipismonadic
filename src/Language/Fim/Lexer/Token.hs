module Language.Fim.Lexer.Token ( Token(..)
                                , teq
                                , tIdentifier
                                , tStringLiteral
                                , tCharLiteral
                                , tNumberLiteral
                                ) where

import qualified Data.Text as T

data Token = ClassStart
           | ClassEnd
           -- Methods
           | MainMethod
           | MethodDec
           | MethodDecEnd
           -- Identifier
           | Identifier T.Text
           -- Literals
           | StringLiteral T.Text
           | CharLiteral Char
           | NumberLiteral Double
           -- Punctuation
           | QuestionMark
           | ExclamationPoint
           | FullStop
           | Newline
           | Colon
           | Comma
           | Ellipsis
           | Interrobang
           -- Variables
           | VariableDec
           | VariableVerb
           | NumberType
           | StringType
           | CharacterType
           -- Arithmetic
           | And
           | AddPrefix
           deriving (Show, Eq)

-- Only match on types
teq :: Token -> Token -> Bool
teq t1 t2 = case (t1, t2) of
  (Identifier{}, Identifier{}) ->       True
  (StringLiteral{}, StringLiteral{}) -> True
  (CharLiteral{}, CharLiteral{}) ->     True
  (NumberLiteral{}, NumberLiteral{}) -> True
  (_, _) -> t1 == t2

-- TODO kinda hacky

tIdentifier :: Token
tIdentifier = Identifier T.empty

tStringLiteral :: Token
tStringLiteral = StringLiteral T.empty

tNumberLiteral :: Token
tNumberLiteral = NumberLiteral 0

tCharLiteral :: Token
tCharLiteral = CharLiteral '\0'
