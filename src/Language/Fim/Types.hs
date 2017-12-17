module Language.Fim.Types ( Class(..)
                          , Function(..)
                          , Statement(..)
                          , Value(..)
                          , Literal(..)
                          , StringQuote(..)
                          , Identifier(..)
                          , Variable(..)
                          ) where

import Data.Text as T

data Class = Class { className :: Identifier
                   , classSuper :: Class
                   , classBody :: [Function]
                   }
           | Celestia
           | ClassByName T.Text
           deriving (Eq, Show)


data Function = Function { functionName :: Identifier
                         , isMain :: Bool
                         , functionBody :: [Statement]
                         } deriving (Eq, Show)

data Statement = Output { outputValue :: Value
                        }
               | Declaration { declareName :: Variable
                             , declareValue :: Value
                             , declareIsConsnant :: Bool
                             }
               deriving (Eq, Show)

data Value = VLiteral { vLiteral :: Literal}
           | VVariable { vVariable :: Variable}
           | VNull
  deriving (Eq, Show)

newtype Variable = Variable { vName :: T.Text } deriving (Eq, Show)

-- TODO: Literals can be prefixed with 'the word,' 'the number,' etc
data Literal = StringLiteral { slValue :: T.Text
                             -- TODO: wrapping quotes don't have to be the same
                             }
             | NumberLiteral { nlValue :: Double} -- TODO: Supposed to be Float64
             | CharacterLiteral { clValue :: Char
                                }
             | Null deriving Show

-- Manually define Eq so our tests don't barf on the doubles
instance Eq Literal where
  (StringLiteral val1) == (StringLiteral val2) = val1 == val2
  (CharacterLiteral val1) == (CharacterLiteral val2) = val1 == val2
  Null == Null = True
  (NumberLiteral n1) == (NumberLiteral n2) = abs (n1 - n2) < 0.00000001
  _ == _ = False


data StringQuote = SimpleQuote | FancyQuote deriving (Eq, Show)

data Identifier = Identifier { idName :: T.Text
                             } deriving (Eq, Show)
