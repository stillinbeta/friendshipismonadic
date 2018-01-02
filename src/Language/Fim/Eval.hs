{-# LANGUAGE FlexibleContexts, ConstraintKinds, FlexibleInstances#-}

module Language.Fim.Eval ( runClass
                         , runClassIO
                         ) where

import Language.Fim.Types
import Language.Fim.Eval.Types ( Evaluator
                               , newEvalState
                               , InputOutput(..)
                               )
import qualified Language.Fim.Eval.Errors as Errors
import Language.Fim.Eval.Statement (evalStatement)

import Control.Monad.State (runStateT, StateT)
import Control.Monad.RWS (execRWST, RWST, tell, ask)
import Control.Monad.Except (runExceptT, ExceptT)
import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.List (find)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

instance InputOutput (StateT s (ExceptT e IO)) where
  getText = liftIO TIO.getLine
  putText = liftIO . TIO.putStrLn

runClassIO :: Class -> IO (Maybe String)
runClassIO cls = do
  val <- runExceptT (runStateT (evalClass cls) newEvalState)
  case val of
    Left errMsg -> return . Just $ T.unpack errMsg
    Right _ -> return Nothing

instance Monad m => InputOutput (RWST T.Text T.Text s m) where
  -- TODO: can only read one line of input per evaluation cycle
  getText = ask
  putText = tell . (`T.snoc` '\n')

runClass :: Class -> T.Text  -> Either T.Text T.Text
runClass cls input =
  snd <$> execRWST (evalClass cls) input newEvalState

evalClass :: (Evaluator m) => Class -> m ()
evalClass cls = do
  let maybeFunc = find isMain $ classBody cls
  case maybeFunc of
    Just method -> evalMainMethod method
    Nothing -> throwError Errors.noMainMethod

evalMainMethod :: (Evaluator m) => Function -> m ()
evalMainMethod mthd = mapM_ evalStatement $ functionBody mthd
