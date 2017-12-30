{-# LANGUAGE FlexibleContexts, OverloadedStrings, ConstraintKinds #-}

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

type Evaluator m = (MonadState EvalState m, MonadError T.Text m, MonadIO m)

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
    Nothing -> throwError "no main method"

evalMainMethod :: (Evaluator m) => Function -> m ()
evalMainMethod mthd = mapM_ evalStatement $ functionBody mthd

evalStatement :: (Evaluator m) => Statement -> m ()
evalStatement o@Output{} = do
  box <- evalValue $ outputValue o
  liftIO . putStrLn . printableLiteral $ box
evalStatement d@Declaration{} = do
  box <- maybe (pure NullBox) evalValue (declareVal d)
  let vname = vName . declareName $ d
  -- TODO: hang on to version information
  checkType box (declareType d) vname
  -- Get a type from the box, if we don't have one already
  let typ = fromMaybe (typeForBox box) (declareType d)
  let vbox = VariableBox { vboxValue = box
                         , vboxIsConstant = declareIsConstant d
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
      box <- evalValue $ assignmentExpr a
      checkType box (vboxType var) aname
      let m' = Map.insert aname (var { vboxValue = box }) m
      modify $ \s -> s { variables = m' }

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

evalBinOp :: (Evaluator m) => ValueBox -> ValueBox -> BinaryOperator -> m ValueBox
evalBinOp v1 v2 binOp
  | binOp `elem` [Add, Multiply, Subtract, Divide] = do
      n1 <- numberOrError v1
      n2 <- numberOrError v2
      return $ NumberBox $ case binOp of
        Add      -> n1 + n2
        Subtract -> n1 - n2
        Multiply -> n1 * n2
        Divide   -> n1 / n2
  | otherwise = undefined

numberOrError :: (Evaluator m) => ValueBox -> m Double
numberOrError (NumberBox n) = pure n
numberOrError v = throwError $ T.intercalate " " ["expected"
                                                 , T.pack . show $ v
                                                 , "to be a number"]

checkType :: (Evaluator m) => ValueBox -> Maybe Type -> T.Text -> m ()
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
    NumberBox num -> T.pack . showNumber $ num
    CharacterBox c -> T.singleton c
    NullBox -> "nothing"

-- Output integral-ish numbers without trailing zeros
showNumber :: Double -> String
showNumber n = let n' = round n :: Int in
  if abs (n - fromIntegral n') < 0.00000001
  then show n'
  else show n
