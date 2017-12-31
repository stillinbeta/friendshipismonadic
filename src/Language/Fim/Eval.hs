{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}

module Language.Fim.Eval (runClass
                         ) where

import Language.Fim.Types
import Language.Fim.Eval.Types ( Evaluator
                               , newEvalState
                               )
import qualified Language.Fim.Eval.Errors as Errors
import Language.Fim.Eval.Statement (evalStatement)

import Control.Monad.State (runStateT)
import Control.Monad.Except (runExceptT)
import Control.Monad.Error.Class (throwError)
import Data.List (find)
import Data.Text.IO (hPutStrLn)
import System.IO (stderr)

runClass :: Class -> IO ()
runClass cls = do
  val <- runExceptT (runStateT (evalClass cls) newEvalState)
  case val of
    Left errMsg -> hPutStrLn stderr errMsg
    Right _ -> return ()

evalClass :: (Evaluator m) => Class -> m ()
evalClass cls = do
  let maybeFunc = find isMain $ classBody cls
  case maybeFunc of
    Just method -> evalMainMethod method
    Nothing -> throwError Errors.noMainMethod

evalMainMethod :: (Evaluator m) => Function -> m ()
evalMainMethod mthd = mapM_ evalStatement $ functionBody mthd
