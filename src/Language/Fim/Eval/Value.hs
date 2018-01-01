{-# LANGUAGE FlexibleContexts #-}

module Language.Fim.Eval.Value (evalValue) where

import Language.Fim.Eval.Types (Evaluator, ValueBox(..), vboxValue, boxLiteral, variables)
import qualified Language.Fim.Eval.Errors as Errors
import Language.Fim.Eval.Util (printableLiteral)
import Language.Fim.Types

import Control.Monad.State.Class (gets)
import Control.Monad.Error.Class (throwError)
import qualified Data.Map as Map

evalValue :: (Evaluator m) => Value -> m ValueBox
evalValue v = case v of
               VVariable { vVariable = var } -> lookupVariable var
               VLiteral { vLiteral = lit } -> return $ boxLiteral lit
               VBinaryOperation { vBinArg1 = v1
                                , vBinOpr  = opr
                                , vBinArg2 = v2
                                } -> do
                 v1' <- evalValue v1
                 v2' <- evalValue v2
                 evalBinOp v1' v2' opr
               VUnaryOperation { vUnArg = v1
                               , vUnOpr = opr
                               } -> do
                 v1' <- evalValue v1
                 evalUnOp v1' opr

evalUnOp :: (Evaluator m) => ValueBox -> UnaryOperator -> m ValueBox
evalUnOp v op = case op of
  Not -> do
    b <- boolOrError v
    return $ BooleanBox $ not b

evalBinOp :: (Evaluator m) => ValueBox -> ValueBox -> BinaryOperator -> m ValueBox
evalBinOp v1 v2 binOp =
  case binOp of
    Add      -> doMath (+)
    Multiply -> doMath (*)
    Subtract -> doMath (-)
    Divide   -> doMath (/)

    EqualTo            -> doComparison (==EQ)
    NotEqualTo         -> doComparison (/=EQ)
    LessThan           -> doComparison (==LT)
    LessThanOrEqual    -> doComparison (/=GT)
    GreaterThan        -> doComparison (==GT)
    GreaterThanOrEqual -> doComparison (/=LT)

    Or -> doBool (||)
    Xor -> doBool (/=)
    And -> case (v1, v2) of
             (BooleanBox b1, BooleanBox b2) -> return $ BooleanBox (b1 && b2)
             (NumberBox n1, NumberBox n2)   -> return $ NumberBox  (n1  + n2)
             (_, _) -> throwError $ Errors.cantDeduceAnd v1 v2
  where
    doMath opr = NumberBox <$>
       (opr <$> numberOrError v1 <*> numberOrError v2)
    doComparison cmp = BooleanBox . cmp <$> compareBox v1 v2
    doBool opr = BooleanBox <$>
      (opr <$> boolOrError v1 <*> boolOrError v2)

numberOrError :: (Evaluator m) => ValueBox -> m Double
numberOrError (NumberBox n) = pure n
numberOrError v = throwError $ Errors.unexpectedType v TNumber

boolOrError :: (Evaluator m) => ValueBox -> m Bool
boolOrError (BooleanBox b) = pure b
boolOrError v = throwError $ Errors.unexpectedType v TBoolean

compareBox :: (Evaluator m) => ValueBox -> ValueBox -> m Ordering
compareBox v1 v2 =
  case (v1, v2) of
    (NullBox, _) -> throwError Errors.compareNull
    (_, NullBox) -> throwError Errors.compareNull
    (NumberBox n1,    NumberBox n2)    -> pure (n1 `compare` n2)
    (CharacterBox c1, CharacterBox c2) -> pure (c1 `compare` c2)
    (StringBox s1,    StringBox s2)    -> pure (s1 `compare` s2)
    (BooleanBox b1,   BooleanBox b2)   -> pure (b1 `compare` b2)
    (_, _) -> pure (printableLiteral v1 `compare` printableLiteral v2)

lookupVariable :: (Evaluator m) => Variable -> m ValueBox
lookupVariable v= do
  let varName = vName v
  maybeVal <- gets $ Map.lookup varName . variables
  case maybeVal of
    Just val -> return $ vboxValue val
    Nothing -> throwError $ Errors.undefinedVariable v
