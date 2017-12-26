module Language.Fim.Eval.Types ( ValueBox(..)
                               , boxLiteral
                               ) where

import Language.Fim.Types (Literal(..))

import qualified Data.Text as T

data ValueBox = NumberBox Double
              | ArrayBox [ValueBox]
              | CharacterBox Char
              | StringBox T.Text
              | BooleanBox Bool
              | NullBox
                deriving (Eq, Show)

boxLiteral :: Literal -> ValueBox
boxLiteral literal = case literal of
  StringLiteral    {slValue = s} -> StringBox s
  NumberLiteral    {nlValue = d} -> NumberBox d
  CharacterLiteral {clValue = c} -> CharacterBox c
