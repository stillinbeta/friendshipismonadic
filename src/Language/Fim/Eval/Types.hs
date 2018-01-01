{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}

module Language.Fim.Eval.Types ( ValueBox(..)
                               , VariableBox(..)
                               , boxLiteral
                               , Evaluator
                               , EvalState(..)
                               , newEvalState
                               , typeForBox
                               , InputOutput(..)
                               ) where

import Language.Fim.Types (Literal(..), Type(..))

import Control.Monad.State.Class (MonadState)
import Control.Monad.Error.Class (MonadError)
import qualified Data.Map as Map
import qualified Data.Text as T

class Monad m => InputOutput m where
  getText :: m T.Text
  putText :: T.Text -> m ()

type Evaluator m = (MonadState EvalState m, MonadError T.Text m, InputOutput m)

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
