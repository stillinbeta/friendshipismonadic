{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Language.Fim.Eval (runClass
                         ) where

import Language.Fim.Types

import Prelude hiding (putStrLn)
import Control.Monad.State.Class
import Control.Monad.Trans.State
import Control.Monad.Error.Class
import Control.Monad.Except
import qualified Data.Map as Map
import Data.List (find)
import qualified Data.Text as T
import Data.Text.IO (putStrLn)
import System.IO (stderr, hPutStrLn)

data EValue = EValueString Int

data EvalState = EvalState { variables :: (Map.Map String EValue)
                           }

newEvalState :: EvalState
newEvalState = EvalState { variables = Map.empty
                         }

runClass :: Class -> IO ()
runClass cls = do
  val <- runExceptT (runStateT (evalClass cls) newEvalState)
  case val of
    Left errMsg -> hPutStrLn stderr errMsg
    Right _ -> return ()

evalClass :: (MonadState EvalState m, MonadError String m, MonadIO m) => Class -> m ()
evalClass cls = do
  let maybeFunc = find isMain $ classBody cls
  case maybeFunc of
    Just method -> evalMainMethod method
    Nothing -> throwError "no main method"


evalMainMethod :: (MonadState EvalState m, MonadError String m, MonadIO m) => Function -> m ()
evalMainMethod mthd = mapM_ evalStatement $ functionBody mthd

evalStatement :: (MonadState EvalState m, MonadError String m, MonadIO m) => Statement -> m ()
evalStatement o@Output{} =
  case outputValue o of
    VVariable{} -> throwError "not inmplemented"
    VLiteral { vLiteral = lit } -> liftIO . putStrLn . printableLiteral $ lit

printableLiteral ::  Literal -> T.Text
printableLiteral literal =
  case literal of
    StringLiteral { slValue = str} -> str
    NumberLiteral { nlValue = num} -> T.pack $ show num
    CharacterLiteral { clValue = chr } -> T.singleton chr
    Null -> "nothing"
