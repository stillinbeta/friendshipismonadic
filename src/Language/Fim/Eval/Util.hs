{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Language.Fim.Eval.Util ( printableLiteral
                              , checkType
                              ) where

import Language.Fim.Eval.Types (Evaluator, ValueBox(..))
import qualified Language.Fim.Eval.Errors as Errors
import Language.Fim.Types

import qualified Data.Text as T
import Control.Monad.Error.Class (throwError)

checkType :: (Evaluator m) => ValueBox -> Maybe Type -> Variable -> m ()
checkType box typ var =
  case (box, typ) of
    (_, Nothing) -> return ()
    (NumberBox{},    Just TNumber)    -> return ()
    (StringBox{},    Just TString)    -> return ()
    (CharacterBox{}, Just TCharacter) -> return ()
    (BooleanBox{},   Just TBoolean)   -> return ()
    (NullBox, _) -> return ()
    (_, Just typ') -> throwError $ Errors.variableTypeMismatch box var typ'

printableLiteral ::  ValueBox -> T.Text
printableLiteral literal =
  case literal of
    StringBox s    -> s
    NumberBox num  -> T.pack . showNumber $ num
    CharacterBox c -> T.singleton c
    BooleanBox b   -> if b then "true" else "false"
    NullBox        -> "nothing"
    ArrayBox _     -> undefined
  where -- Output integral-ish numbers without trailing zeros
    showNumber n = let n' = round n :: Int in
      if abs (n - fromIntegral n') < 0.00000001
      then show n'
      else show n
