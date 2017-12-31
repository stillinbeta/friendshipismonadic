{-# LANGUAGE FlexibleContexts, OverloadedStrings, ConstraintKinds #-}

module Language.Fim.Eval (runClass
                         ) where

import Language.Fim.Types
import Language.Fim.Eval.Types (VariableBox(..), ValueBox(..), boxLiteral)

import Prelude hiding (putStrLn)
import Control.Applicative ((<|>))
import Control.Monad.State.Class
import Control.Monad.State (runStateT)
import Control.Monad.Error.Class
import Control.Monad.Except
import qualified Data.Map as Map
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
  when
    (Map.member vname m)
    $ throwError (T.concat ["can't redefine variable "
                           , vname
                           ])
  let m' = Map.insert vname vbox m
  modify $ \s -> s {variables = m' }
evalStatement a@Assignment{} = do
  let aVar =  assignmentName a
  let aVarName = vName aVar
  mvar <- gets $ Map.lookup aVarName . variables
  case mvar of
    Nothing -> throwError $ T.concat ["Undefined variable ", showVariable aVar ]
    Just var -> do
      when (vboxIsConstant var) $
        throwError $ T.concat [ "can't redefine constant "
                              , showVariable aVar ]
      m <- gets variables
      box <- evalValue $ assignmentExpr a
      checkType box (vboxType var) aVar
      let m' = Map.insert aVarName (var { vboxValue = box }) m
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
        _ -> undefined
  | isComparison binOp = do
     ord <- compareBox v1 v2
     return $ BooleanBox $ case ord of
       -- rolls eyes at haskell type system
       LT -> binOp `elem` [NotEqualTo, LessThanOrEqual, LessThan]
       GT -> binOp `elem` [NotEqualTo, GreaterThanOrEqual, GreaterThan]
       EQ -> binOp `elem` [EqualTo, LessThanOrEqual, GreaterThanOrEqual]
  | otherwise = undefined
  where isComparison = (`elem` [EqualTo, NotEqualTo
                               , GreaterThan, GreaterThanOrEqual
                               , LessThan, LessThanOrEqual])

numberOrError :: (Evaluator m) => ValueBox -> m Double
numberOrError (NumberBox n) = pure n
numberOrError v = throwError $
  T.intercalate " " ["expected" , T.pack . show $ v , "to be a number"]

compareBox :: (Evaluator m) => ValueBox -> ValueBox -> m Ordering
compareBox v1 v2 =
  case (v1, v2) of
    (NullBox, _) -> throwError "can't compare null"
    (_, NullBox) -> throwError "can't compare null"
    (NumberBox n1,    NumberBox n2)    -> pure (n1 `compare` n2)
    (CharacterBox c1, CharacterBox c2) -> pure (c1 `compare` c2)
    (StringBox s1,    StringBox s2)    -> pure (s1 `compare` s2)
    (BooleanBox b1,   BooleanBox b2)   -> pure (b1 `compare` b2)
    (_, _) -> pure (printableLiteral v1 `compare` printableLiteral v2)


checkType :: (Evaluator m) => ValueBox -> Maybe Type -> Variable -> m ()
checkType box typ var =
  case (box, typ) of
    (_, Nothing) -> return ()
    (NumberBox{},    Just TNumber)    -> return ()
    (StringBox{},    Just TString)    -> return ()
    (CharacterBox{}, Just TCharacter) -> return ()
    (BooleanBox{},   Just TBoolean)   -> return ()
    (NullBox, _) -> return ()
    (_, Just typ') -> throwError $
      T.intercalate " " ["Can't assign"
                        , boxTypeName box
                        , "to variable"
                        , showVariable var
                        , "of type"
                        , typeName typ'
                        ]

typeForBox :: ValueBox -> Maybe Type
typeForBox box = case box of
  NumberBox{}    -> Just TNumber
  StringBox{}    -> Just TString
  CharacterBox{} -> Just TCharacter
  BooleanBox{}   -> Just TBoolean
  NullBox{}      -> Nothing
  -- TODO
  ArrayBox{}     -> undefined

boxTypeName :: ValueBox -> T.Text
boxTypeName = maybe "nothing" typeName . typeForBox

typeName :: Type -> T.Text
typeName typ = case typ of
  TNumber    -> "number"
  TString    -> "string"
  TCharacter -> "character"
  TBoolean   -> "argument"

lookupVariable :: (MonadState EvalState m, MonadError T.Text m) => Variable -> m ValueBox
lookupVariable v= do
  let varName = vName v
  maybeVal <- gets $ Map.lookup varName . variables
  case maybeVal of
    Just val -> return $ vboxValue val
    Nothing -> throwError $ T.concat ["undefined variable ", showVariable v]

showVariable :: Variable -> T.Text
showVariable Variable {vName = v} = T.concat ["<", v, ">"]

printableLiteral ::  ValueBox -> T.Text
printableLiteral literal =
  case literal of
    StringBox s    -> s
    NumberBox num  -> T.pack . showNumber $ num
    CharacterBox c -> T.singleton c
    BooleanBox b   -> if b then "true" else "false"
    NullBox        -> "nothing"
    ArrayBox _     -> undefined

-- Output integral-ish numbers without trailing zeros
showNumber :: Double -> String
showNumber n = let n' = round n :: Int in
  if abs (n - fromIntegral n') < 0.00000001
  then show n'
  else show n
