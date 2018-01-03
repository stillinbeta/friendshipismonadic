{-# LANGUAGE FlexibleContexts #-}

module Language.Fim.Eval.Statement (evalStatement) where

import Language.Fim.Eval.Types ( VariableBox(..) , ValueBox(..)
                               , Evaluator , variables, methods, typeForBox
                               , putText, getText, boxLiteral
                               )
import Language.Fim.Types
import qualified Language.Fim.Eval.Errors as Errors
import Language.Fim.Eval.Util (printableLiteral, boxInput, checkType)

import Prelude hiding (putStrLn)
import Control.Applicative ((<|>))
import Control.Monad (when, void)
import Control.Monad.Error.Class (throwError)
import Control.Monad.State.Class (gets, modify)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Class (lift)
import qualified Data.Map as Map
import Data.Ix (range)


evalMethod :: Evaluator m => [Statement] -> m ValueBox
evalMethod stmts = do
  vars <- gets variables
  modify (\s -> s { variables = Map.empty})
  mapM_ evalStatement stmts
  modify (\s -> s {variables = vars})
  return $ NullBox

evalStatement :: (Evaluator m) => Statement -> m ()
evalStatement o@Output{} = do
  box <- evalValue $ outputValue o
  putText $ printableLiteral box
evalStatement i@Input{} = do
  text <- getText
  let box = boxInput text
  declareVariable (inputName i) box False (inputType i)
evalStatement p@Prompt{} = do
  evalStatement Output { outputValue = promptVal p}
  evalStatement Input { inputName = promptName p
                      , inputType = Nothing
                      }

evalStatement d@Declaration{} = do
  box <- maybe (pure NullBox) evalValue (declareVal d)
  declareVariable (declareName d) box (declareIsConstant d) (declareType d)
evalStatement c@Call{} =
  void . evalValue . callVal $ c

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

-- evalStatement Declare{} without the evaling, used by Declare{}, Prompt{}, and Input{}
declareVariable :: Evaluator m => Variable -> ValueBox -> Bool -> Maybe Type -> m ()
declareVariable var box isConstant typ = do
-- TODO: hang on to version information
  checkType box typ var
  let vname = vName var
  -- Try to get a type from the box, if we don't have one already
  let typ' = typ <|> typeForBox box
  let vbox = VariableBox { vboxValue = box
                         , vboxIsConstant = isConstant
                         , vboxType = typ'
                         }
  m <- gets variables
  when (Map.member vname m) $ throwError (Errors.redeclaredVariable var)
  let m' = Map.insert vname vbox m
  modify $ \s -> s {variables = m' }

-- Does not perform any checking!
-- will not set new
setVariable :: (Evaluator m) => Variable -> ValueBox -> m ()
setVariable var box = do
  let vname = vName var
  m <- gets variables
  let m' = Map.adjust (\v -> v { vboxValue = box}) vname m
  modify $ \s -> s {variables = m' }


-- Value was originally its own module But evaluating methods made that module
-- mutually recursive with this one and that's no good

evalValue :: (Evaluator m) => Value -> m ValueBox
evalValue v = case v of
               VVariable { vVariable = var } -> lookupIdentifier var
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

lookupIdentifier :: (Evaluator m) => Variable -> m ValueBox
lookupIdentifier idt = do
  meth <- lookupMethod idt
  var <- lookupVariable idt
  case (meth, var) of
    (Just val, _) -> return val
    (_, Just val) -> return val
    _ -> throwError (Errors.undefinedVariable idt)

lookupVariable :: (Evaluator m) => Variable -> m (Maybe ValueBox)
lookupVariable Variable {vName = varName } = do
  mv <- gets $ Map.lookup varName . variables
  return $ vboxValue <$> mv

lookupMethod :: (Evaluator m) => Variable -> m (Maybe ValueBox)
lookupMethod Variable {vName = varName} = do
  mf <- gets (Map.lookup varName . methods)
  case mf of
    Just f -> Just <$> evalMethod (functionBody f)
    Nothing -> return Nothing
