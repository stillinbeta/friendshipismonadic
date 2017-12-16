module Language.Fim.Types ( Class(..)
                          , Function(..)
                          , Statement(..)
                          , Value(..)
                          , Literal(..)
                          , StringQuote(..)
                          , Identifier(..)
                          , Terminator(..)
                          , OutputVerb(..)
                          , DeclareVerb(..)
                          , Variable(..)
                          , Article(..)
                          , TypeNoun(..)
                          ) where

import Data.Text as T

data Class = Class { className :: Identifier
                   , classSuper :: Class
                   , classBody :: [Function]
                   , classStudent :: Identifier
                   }
           | Celestia
           | ClassByName T.Text
           deriving (Eq, Show)


data Function = Function { functionName :: Identifier
                         , isMain :: Bool
                         , functionBody :: [Statement]
                         } deriving (Eq, Show)

data Statement = Output { outputVerb :: OutputVerb
                        , outputValue :: Value
                        , outputTerminator :: Terminator
                        }
               | Declaration { declareVerb :: DeclareVerb
                             , declareName :: Variable
                             , declareValue :: Value
                             , declareIsConsnant :: Bool
                             , declareTypeArticle :: Maybe Article
                             , declareTypeNoun :: TypeNoun
                             }
               deriving (Eq, Show)

data Article = The | A | An deriving (Eq, Show)

data TypeNoun = Number -- numbers
              | Letter | Character -- character
              | Word | Phrase | Sentence | Quote | Name -- string
              | Logic | Argument  -- boolean
              deriving (Eq, Show)

data DeclareVerb = Is | Was | Has | Had | Like | Likes | Liked deriving (Eq, Show)

data OutputVerb = Sang | Wrote | Said | Thought deriving (Eq, Show)

data Value = VLiteral { vLiteral :: Literal}
           | VVariable { vVariable :: Variable}
           | VNull
  deriving (Eq, Show)

newtype Variable = Variable { vName :: T.Text } deriving (Eq, Show)

-- TODO: Literals can be prefixed with 'the word,' 'the number,' etc
data Literal = StringLiteral { slValue :: T.Text
                             -- TODO: wrapping quotes don't have to be the same
                             , slWrap :: StringQuote
                             }
             | NumberLiteral { nlValue :: Double} -- TODO: Supposed to be Float64
             | CharacterLiteral { clValue :: Char
                                , clWrap :: StringQuote
                                }
             | Null deriving Show

-- Manually define Eq so our tests don't barf on the doubles
instance Eq Literal where
  (StringLiteral val1 wrap1) == (StringLiteral val2 wrap2) = val1 == val2 && wrap1 == wrap2
  (CharacterLiteral val1 wrap1) == (CharacterLiteral val2 wrap2) = val1 == val2 && wrap1 == wrap2
  Null == Null = True
  (NumberLiteral n1) == (NumberLiteral n2) = abs (n1 - n2) < 0.00000001
  _ == _ = False


data StringQuote = SimpleQuote | FancyQuote deriving (Eq, Show)

data Identifier = Identifier { idName :: T.Text
                             , idTerminator :: Terminator
                             } deriving (Eq, Show)

data Terminator = FullStop | Comma | QuestionMark | Exclamation deriving (Eq, Show)
