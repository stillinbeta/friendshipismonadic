{-# LANGUAGE FlexibleContexts #-}

module Language.Fim.Eval.Statement (evalStatement) where

import Language.Fim.Eval.Types ( VariableBox(..) , ValueBox(..)
                               , Evaluator , variables , typeForBox)
import Language.Fim.Types
import qualified Language.Fim.Eval.Errors as Errors
import Language.Fim.Eval.Value (evalValue)
import Language.Fim.Eval.Util (printableLiteral, checkType)

import Prelude hiding (putStrLn)
import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Error.Class (throwError)
import Control.Monad.State.Class (gets, modify)
import qualified Data.Map as Map
import Data.Text.IO (putStrLn)


evalStatement :: (Evaluator m) => Statement -> m ()
evalStatement o@Output{} = do
  box <- evalValue $ outputValue o
  liftIO . putStrLn . printableLiteral $ box
evalStatement d@Declaration{} = do
  box <- maybe (pure NullBox) evalValue (declareVal d)
  let var = declareName d
  let vname = vName var
  -- TODO: hang on to version information
  checkType box (declareType d) var
  -- Try to get a type from the box, if we don't have one already
  let typ = (declareType d) <|> (typeForBox box)
  let vbox = VariableBox { vboxValue = box
                         , vboxIsConstant = declareIsConstant d
                         , vboxType = typ
                         }
  m <- gets variables
  when (Map.member vname m) $ throwError (Errors.redeclaredVariable var)
  let m' = Map.insert vname vbox m
  modify $ \s -> s {variables = m' }
evalStatement a@Assignment{} = do
  let aVar =  assignmentName a
  let aVarName = vName aVar
  mvar <- gets $ Map.lookup aVarName . variables
  case mvar of
    Nothing -> throwError $ Errors.undefinedVariable aVar
    Just var -> do
      when (vboxIsConstant var) $ throwError (Errors.assignToConstant aVar)
      m <- gets variables
      box <- evalValue $ assignmentExpr a
      checkType box (vboxType var) aVar
      let m' = Map.insert aVarName (var { vboxValue = box }) m
      modify $ \s -> s { variables = m' }
