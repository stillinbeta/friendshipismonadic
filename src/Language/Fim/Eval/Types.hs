module Language.Fim.Eval.Types ( ValueBox(..)
                               , VariableBox(..)
                               , boxLiteral
                               ) where

import Language.Fim.Types (Literal(..), Type)

import qualified Data.Text as T

data VariableBox = VariableBox { vboxValue      :: ValueBox
                               , vboxIsConstant :: Bool
                               , vboxType       :: Maybe Type
                               }

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
  BooleanLiteral   {blValue = b} -> BooleanBox b
  NullLiteral                    -> NullBox
