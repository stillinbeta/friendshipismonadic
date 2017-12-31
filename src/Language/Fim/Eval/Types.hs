{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

module Language.Fim.Eval.Types ( ValueBox(..)
                               , VariableBox(..)
                               , boxLiteral
                               , Evaluator
                               , EvalState(..)
                               , newEvalState
                               , typeForBox
                               ) where

import Language.Fim.Types (Literal(..), Type(..))

import Control.Monad.State.Class (MonadState)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Map as Map
import qualified Data.Text as T

type Evaluator m = (MonadState EvalState m, MonadError T.Text m, MonadIO m)

data EvalState = EvalState { variables :: Map.Map T.Text VariableBox
                           }
newEvalState :: EvalState
newEvalState = EvalState { variables = Map.empty
                         }

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

typeForBox :: ValueBox -> Maybe Type
typeForBox box = case box of
  NumberBox{}    -> Just TNumber
  StringBox{}    -> Just TString
  CharacterBox{} -> Just TCharacter
  BooleanBox{}   -> Just TBoolean
  NullBox{}      -> Nothing
  -- TODO
  ArrayBox{}     -> undefined
