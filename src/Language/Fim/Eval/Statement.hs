{-# LANGUAGE FlexibleContexts #-}

module Language.Fim.Eval.Statement (evalStatement) where

import Language.Fim.Eval.Types ( VariableBox(..) , ValueBox(..)
                               , Evaluator , variables , typeForBox
                               , putText --, getText
                               )
import Language.Fim.Types
import qualified Language.Fim.Eval.Errors as Errors
import Language.Fim.Eval.Value (evalValue, boolOrError)
import Language.Fim.Eval.Util (printableLiteral, checkType)

import Prelude hiding (putStrLn)
import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.Error.Class (throwError)
import Control.Monad.State.Class (gets, modify)
import qualified Data.Map as Map
import Data.Ix (range)


evalStatement :: (Evaluator m) => Statement -> m ()
evalStatement o@Output{} = do
  box <- evalValue $ outputValue o
  putText $ printableLiteral box
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
      box <- evalValue $ assignmentExpr a
      checkType box (vboxType var) aVar
      setVariable aVar box
evalStatement i@IfThenElse{} = do
  box <- evalValue $ ifOnVal i
  branch <- boolOrError box
  let stmts = if branch
              then ifThen i
              else ifElse i
  -- TODO should if/then/else be scoped?
  mapM_ evalStatement stmts

evalStatement w@While{} = do
  box <- evalValue $ whileVal w
  branch <- boolOrError box
  when branch $ do
    mapM_ evalStatement $ whileBody w
    evalStatement w

evalStatement w@DoWhile{} = do
  mapM_ evalStatement $ doWhileBody w
  box <- evalValue $ doWhileVal w
  branch <- boolOrError box
  when branch $ evalStatement w

evalStatement f@For{} = do
  from <- evalValue $ forFrom f
  to <- evalValue $ forTo f
  let var = forVar f
  let typ = forType f
  vals <- boxRange from to
  evalStatement Declaration { declareName = var
                            , declareVal = Nothing
                            , declareIsConstant = False
                            , declareType = Just typ
                            }
  let doIter val = do
        setVariable var val
        mapM_ evalStatement $ forBody f
  mapM_ doIter vals
  where
    boxRange from to = case (from, to) of
      (NumberBox n1, NumberBox n2) ->
        let n1' = round n1 :: Int
            n2' = round n2 :: Int in
          pure $ map (NumberBox . fromIntegral) (range (n1', n2'))
      (CharacterBox c1, CharacterBox c2) -> pure $ map CharacterBox (range (c1, c2))
      (_, _) -> throwError $ Errors.cantDeduceAnd from to


-- Does not perform any checking!
-- will not set new
setVariable :: (Evaluator m) => Variable -> ValueBox -> m ()
setVariable var box = do
  let vname = vName var
  m <- gets variables
  let m' = Map.adjust (\v -> v { vboxValue = box}) vname m
  modify $ \s -> s {variables = m' }
