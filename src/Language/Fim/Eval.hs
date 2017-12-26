{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Language.Fim.Eval (runClass
                         ) where

import Language.Fim.Types
import Language.Fim.Eval.Types (ValueBox(..), boxLiteral)

import Prelude hiding (putStrLn)
import Control.Applicative (empty)
import Control.Monad.State.Class
import Control.Monad.State (runStateT)
import Control.Monad.Error.Class
import Control.Monad.Except
import qualified Data.Map as Map
import Data.List (find)
import qualified Data.Text as T
import Data.Text.IO (putStrLn, hPutStrLn)
import System.IO (stderr)

data EvalState = EvalState { variables :: Map.Map T.Text ValueBox
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

evalClass :: (MonadState EvalState m, MonadError T.Text m, MonadIO m) => Class -> m ()
evalClass cls = do
  let maybeFunc = find isMain $ classBody cls
  case maybeFunc of
    Just method -> evalMainMethod method
    Nothing -> throwError "no main method"


evalMainMethod :: (MonadState EvalState m, MonadError T.Text m, MonadIO m) => Function -> m ()
evalMainMethod mthd = mapM_ evalStatement $ functionBody mthd

evalStatement :: (MonadState EvalState m, MonadError T.Text m, MonadIO m) => Statement -> m ()
evalStatement o@Output{} = do
  box <- getValue $ outputValue o
  liftIO . putStrLn . printableLiteral $ box
evalStatement d@Declaration{} = do
  box <- getValue $ declareValue d
  -- TODO: hang on to version information
  case (box, declareType d) of
    (_, Nothing) -> return ()
    (NumberBox{}, Just TNumber) -> return ()
    (StringBox{}, Just TString) -> return ()
    (CharacterBox{}, Just TCharacter) -> return ()
    (NullBox, _) -> return ()
    (_, Just{}) -> throwError $ T.concat ["Unexpected type for variable ",
                                                   vName . declareName $ d]
  -- TODO: handle constants
  setValue (declareName d) box

setValue :: MonadState EvalState m => Variable -> ValueBox -> m ()
setValue var val= do
  m <- gets variables
  let m' = Map.insert (vName var) val m
  modify $ \s -> s { variables = m' }

getValue :: (MonadState EvalState m, MonadError T.Text m) => Value -> m ValueBox
getValue v = case v of
               VVariable { vVariable = var } -> lookupVariable var
               VLiteral { vLiteral = lit } -> return $ boxLiteral lit
               VNull -> return NullBox

lookupVariable :: (MonadState EvalState m, MonadError T.Text m) => Variable -> m ValueBox
lookupVariable v= do
  let varName = vName v
  maybeVal <- gets $ Map.lookup varName . variables
  case maybeVal of
    Just val -> return val
    Nothing -> throwError $ T.concat ["undefined variable ", varName]

printableLiteral ::  ValueBox -> T.Text
printableLiteral literal =
  case literal of
    StringBox s -> s
    NumberBox num -> T.pack $ show num
    CharacterBox c -> T.singleton c
    NullBox -> "nothing"
