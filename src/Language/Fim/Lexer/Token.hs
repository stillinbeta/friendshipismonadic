module Language.Fim.Lexer.Token ( Token(..)
                                , teq
                                , tIdentifier
                                , tStringLiteral
                                , tCharLiteral
                                , tNumberLiteral
                                , tIntLiteral
                                ) where

import qualified Data.Text as T

data Token = ClassStart
           | ClassEnd
           -- Methods
           | MainMethod
           | MethodDec
           | MethodReturn
           | MethodArgs
           | MethodDecEnd
           | Call
           | Return
           -- Identifier
           | Identifier {identifier :: T.Text}
           -- Literals
           | StringLiteral {stringLiteral :: T.Text}
           | CharLiteral {charLiteral :: Char}
           | IntLiteral {intLiteral :: Int}
           | NumberLiteral {numberLiteral :: Double}
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
           | Plural
           | Many
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
           | Increment
           | Decrement
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
           -- Switch
           | SwitchStart
           | SwitchCasePrefix
           | SwitchCaseInfix
           | SwitchCaseSuffix
           | SwitchDefault
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
  (Identifier{}, Identifier{})       -> True
  (StringLiteral{}, StringLiteral{}) -> True
  (CharLiteral{}, CharLiteral{})     -> True
  (NumberLiteral{}, NumberLiteral{}) -> True
  (IntLiteral{}, IntLiteral{})       -> True
  (_, _) -> t1 == t2

-- TODO kinda hacky - for matching on with teq
tIdentifier :: Token
tIdentifier = Identifier T.empty

tStringLiteral :: Token
tStringLiteral = StringLiteral T.empty

tNumberLiteral :: Token
tNumberLiteral = NumberLiteral 0

tIntLiteral :: Token
tIntLiteral = IntLiteral 0

tCharLiteral :: Token
tCharLiteral = CharLiteral '\0'
