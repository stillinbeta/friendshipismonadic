{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Language.Fim.Eval (runClass
                         ) where

import Language.Fim.Types
import Language.Fim.Eval.Types (VariableBox(..), ValueBox(..), boxLiteral)

import Prelude hiding (putStrLn)
import Control.Monad.State.Class
import Control.Monad.State (runStateT)
import Control.Monad.Error.Class
import Control.Monad.Except
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.List (find)
import qualified Data.Text as T
import Data.Text.IO (putStrLn, hPutStrLn)
import System.IO (stderr)

data EvalState = EvalState { variables :: Map.Map T.Text VariableBox
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
  box <- evalExpression $ outputExpr o
  liftIO . putStrLn . printableLiteral $ box
evalStatement d@Declaration{} = do
  box <- maybe (pure NullBox) evalExpression (declareExpr d)
  let vname = vName . declareName $ d
  -- TODO: hang on to version information
  checkType box (declareType d) vname
  -- Get a type from the box, if we don't have one already
  let typ = fromMaybe (typeForBox box) (declareType d)
  let vbox = VariableBox { vboxValue = box
                         , vboxIsConstant = declareIsConsnant d
                         , vboxType = Just typ
                         }
  m <- gets variables
  when
    (Map.member vname m)
    $ throwError (T.concat ["can't redefine variable "
                           , vname
                           ])
  let m' = Map.insert vname vbox m
  modify $ \s -> s {variables = m' }
evalStatement a@Assignment{} = do
  let aname = vName . assignmentName $ a
  mvar <- gets $ Map.lookup aname . variables
  case mvar of
    Nothing -> throwError $ T.concat ["Undefined variable ", aname]
    Just var -> do
      when (vboxIsConstant var) $
        throwError $ T.concat [ "can't redefine constant "
                              , aname ]
      m <- gets variables
      box <- evalExpression $ assignmentExpr a
      checkType box (vboxType var) aname
      let m' = Map.insert aname (var { vboxValue = box }) m
      modify $ \s -> s { variables = m' }

evalExpression :: (MonadState EvalState m, MonadError T.Text m) => Expression -> m ValueBox
evalExpression v = case v of
               EVariable { eVariable = var } -> lookupVariable var
               ELiteral { eLiteral = lit } -> return $ boxLiteral lit

checkType :: (MonadError T.Text m) => ValueBox -> Maybe Type -> T.Text -> m ()
checkType box typ varName =
  case (box, typ) of
    (_, Nothing) -> return ()
    (NumberBox{}, Just TNumber) -> return ()
    (StringBox{}, Just TString) -> return ()
    (CharacterBox{}, Just TCharacter) -> return ()
    (NullBox, _) -> return ()
    (_, Just typ') -> throwError $ T.concat ["Can't assign "
                                            , boxTypeName box
                                            , " to variable "
                                            , varName
                                            , " of type "
                                            , typeName typ'
                                            ]

typeForBox :: ValueBox -> Type
typeForBox box = case box of
  NumberBox{}    -> TNumber
  StringBox{}    -> TString
  CharacterBox{} -> TCharacter
  -- TODO
  NullBox{}      -> undefined
  BooleanBox{}   -> undefined
  ArrayBox{}     -> undefined

boxTypeName :: ValueBox -> T.Text
boxTypeName = typeName . typeForBox

typeName :: Type -> T.Text
typeName typ = case typ of
  TNumber    -> "number"
  TString    -> "string"
  TCharacter -> "character"

lookupVariable :: (MonadState EvalState m, MonadError T.Text m) => Variable -> m ValueBox
lookupVariable v= do
  let varName = vName v
  maybeVal <- gets $ Map.lookup varName . variables
  case maybeVal of
    Just val -> return $ vboxValue val
    Nothing -> throwError $ T.concat ["undefined variable ", varName]

printableLiteral ::  ValueBox -> T.Text
printableLiteral literal =
  case literal of
    StringBox s -> s
    NumberBox num -> T.pack $ show num
    CharacterBox c -> T.singleton c
    NullBox -> "nothing"
