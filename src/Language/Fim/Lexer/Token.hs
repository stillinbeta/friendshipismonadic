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
           | Call
           -- Identifier
           | Identifier T.Text
           -- Literals
           | StringLiteral T.Text
           | CharLiteral Char
           | NumberLiteral Double
           | NullLiteral
           | TrueLiteral
           | FalseLiteral
           -- Punctuation
           | QuestionMark
           | ExclamationPoint
           | FullStop
           | Colon
           | Comma
           | Ellipsis
           | Interrobang
           -- Variables
           | VariableDec
           | Article
           | VariableConstant
           -- Types
           | NumberType
           | StringType
           | CharacterType
           | BooleanType
           -- Verbs
           | Is
           | Are
           | Now
           | WasHad
           | Like
           | Liked
           | Become
           -- Interaction
           | OutputVerb
           | PromptVerb
           | InputVerb
           | InputType
           -- Arithmetic
           | And
           | From
           | By
           | AddPrefix
           | AddInfix
           | SubtractInfix
           | SubtractPrefix
           | MultiplyInfix
           | MultiplyPrefix
           | DivideInfix
           | DividePrefix
           -- Comparison
           | Nt
           | Not
           | No
           | Than
           | Less
           | More
           -- Boolean logic
           | Either
           | Or
           | NotTheCase
           -- Conditionals
           | If
           | Then
           | Else
           | Fi
           -- While loops
           | WhileStart
           | DoWhileStart
           | WhileEnd
           | DoWhileEnd
           | DoWhileEnd2
           | ForStart
           | To
           deriving (Show, Eq)

-- Only match on types, not wrapped value
teq :: Token -> Token -> Bool
teq t1 t2 = case (t1, t2) of
  (Identifier{}, Identifier{}) ->       True
  (StringLiteral{}, StringLiteral{}) -> True
  (CharLiteral{}, CharLiteral{}) ->     True
  (NumberLiteral{}, NumberLiteral{}) -> True
  (_, _) -> t1 == t2

-- TODO kinda hacky - for matching on with teq
tIdentifier :: Token
tIdentifier = Identifier T.empty

tStringLiteral :: Token
tStringLiteral = StringLiteral T.empty

tNumberLiteral :: Token
tNumberLiteral = NumberLiteral 0

tCharLiteral :: Token
tCharLiteral = CharLiteral '\0'
