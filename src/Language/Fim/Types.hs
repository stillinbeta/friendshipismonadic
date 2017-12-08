module Language.Fim.Types ( Class(..)
                 , Function(..)
                 , Statement(..)
                 , Value(..)
                 , Literal(..)
                 , StringQuote(..)
                 , Identifier(..)
                 , Terminator(..)
                 , OutputVerb(..)
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
                        } deriving (Eq, Show)

data OutputVerb = Sang | Wrote | Said | Thought deriving (Eq, Show)

data Value = VLiteral Literal deriving (Eq, Show)

data Literal = StringLiteral { slValue :: T.Text
                             , slWrap :: StringQuote
                             }
  | Null deriving (Eq, Show)

data StringQuote = SimpleQuote | FancyQuote deriving (Eq, Show)

data Identifier = Identifier { idName :: T.Text
                             , idTerminator :: Terminator
                             } deriving (Eq, Show)


data Terminator = FullStop | Comma | QuestionMark | Exclamation deriving (Eq, Show)
