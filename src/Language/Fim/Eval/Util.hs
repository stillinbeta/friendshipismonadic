{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Language.Fim.Eval.Util ( printableLiteral
                              , boxInput
                              , checkType
                              , typeMatch
                              ) where

import Language.Fim.Eval.Types (Evaluator, ValueBox(..))
import qualified Language.Fim.Eval.Errors as Errors
import Language.Fim.Types

import Control.Monad (unless)
import qualified Data.Text as T
import Data.Text.Read (signed, rational)
import Control.Monad.Error.Class (throwError)

typeMatch :: ValueBox -> Type -> Bool
typeMatch box typ = case (box, typ) of
    (NumberBox{},    TNumber)    -> True
    (StringBox{},    TString)    -> True
    (CharacterBox{}, TCharacter) -> True
    (BooleanBox{},   TBoolean)   -> True
    _ -> False

checkType :: (Evaluator m) => ValueBox -> Maybe Type -> Identifier -> m ()
checkType box typ var =
  case (box, typ) of
    (_, Nothing)    -> return ()
    (NullBox, _)    -> return ()
    (_, Just typ') -> unless (typeMatch box typ') $
                          throwError (Errors.variableTypeMismatch box (Variable var) typ')

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

boxInput :: T.Text -> ValueBox
boxInput t
  | T.length t == 1 = CharacterBox $ T.head t
  | t `elem` ["yes", "true", "right", "correct"]   = BooleanBox True
  | t `elem` ["no", "false", "wrong", "incorrect"] = BooleanBox False
  | otherwise = case signed rational t of
                  Right (num, "") -> NumberBox num
                  _ -> StringBox t
