{-# LANGUAGE FlexibleContexts #-}

module Language.Fim.Eval.Statement (evalStatements) where

import Language.Fim.Eval.Types ( VariableBox(..) , ValueBox(..)
                               , Evaluator , variables, methods, typeForBox
                               , putText, getText, boxLiteral
                               )
import Language.Fim.Types
import qualified Language.Fim.Eval.Errors as Errors
import Language.Fim.Eval.Util (printableLiteral, boxInput, checkType, typeMatch)

import Control.Applicative ((<|>), empty)
import Control.Monad (when, unless, void, foldM)
import Control.Monad.Error.Class (throwError)
import Control.Monad.State.Class (gets, modify)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Ix (range)
import qualified Data.Text as T

evalMethod :: Evaluator m => Function -> [ValueBox] -> m ValueBox
evalMethod f args = do
  -- save old variables
  vars <- gets variables
  -- blank variables for new method
  modify (\s -> s { variables = Map.empty})
  -- make sure args list is the right length
  when (length args /= length (functionArgs f)) $
    throwError (Errors.methodIncorrectArgCount f args)
  -- populate the method's namespace with the args
  mapM_ populateVars $ zip args (functionArgs f)
  -- eval the method
  res <- evalStatements (functionBody f)
  -- put the old variables back
  modify (\s -> s {variables = vars})
  -- always return something
  let retVal = fromMaybe NullBox res
  let goodReturn = case (retVal, functionReturnType f) of
        (NullBox, Nothing) -> True
        (_, Nothing) -> False
        (_, Just typ) -> typeMatch retVal typ
  if goodReturn
    then return retVal
    else throwError $ Errors.methodIncorrectReturn f retVal

  where
    populateVars (given, expected) = do
      unless (typeMatch given (argType expected)) $
        throwError $ Errors.methodIncorrectArgType f expected given
      declareVariable (Variable $ argName expected) given False (Just $ argType expected)

evalStatements :: Evaluator m => [Statement] -> m (Maybe ValueBox)
evalStatements = foldM evalStatements' Nothing

evalStatements' :: Evaluator m => Maybe ValueBox -> Statement -> m (Maybe ValueBox)
evalStatements' box stmt = pure box <||> evalStatement stmt

evalStatement :: (Evaluator m) => Statement -> m (Maybe ValueBox)
evalStatement o@Output{} = do
  box <- evalValue $ outputValue o
  putText $ printableLiteral box
  noReturn
evalStatement i@Input{} = do
  text <- getText
  let box = boxInput text
  declareVariable (inputName i) box False (inputType i)
  noReturn
evalStatement p@Prompt{} = do
  void $ evalStatement Output { outputValue = promptVal p}
  void $ evalStatement Input { inputName = promptName p
                      , inputType = Nothing
                      }
  noReturn
evalStatement d@Declaration{} = do
  let typ' = declareType d
  vals <- mapM evalValue $ declareVals d
  -- if there's more than one value, definitely an array
  -- if there's only one value but the type is array, it's a singleton
  box <- if length vals > 1 || isArrayType
    then do
      mapM_ checkArrayType $ zip [1..] vals
      return ArrayBox { arrType = typ'
                      , arrVals = vals
                      }
    else return $ case vals of
                    [] -> NullBox
                    -- should always be singleton, but x just to be safe
                    (x:_) -> x
  declareVariable (declareName d) box (declareIsConstant d) typ'
  noReturn
  where
    isArrayType = case declareType d of
      Just (TArray _) -> True
      _ -> False
    checkArrayType (i, box) =
      case declareType d of
        Just (TArray innerType) ->
          unless (typeMatch box innerType) $
            throwError $ Errors.arrayTypeError (declareName d) innerType i box
        _ -> pure ()
evalStatement c@Call{} =
  Nothing <$ evalValue (callVal c)
evalStatement r@Return{} =
  Just <$> evalValue (returnVal r)
evalStatement d@Decrement{} =
  evalStatement $ makeIncrDecr (decVar d) Subtract
evalStatement i@Increment{} =
  evalStatement $ makeIncrDecr (incrVar i) Add
evalStatement a@Assignment{} = do
  let aVar =  assignmentName a
  let aVarName = vName aVar
  mvar <- gets $ Map.lookup aVarName . variables
  case mvar of
    -- No variable found
    Nothing -> throwError $ Errors.undefinedVariable aVar
    -- Variable already defined
    Just var -> do
      when (vboxIsConstant var) $ throwError (Errors.assignToConstant aVar)
      box <- evalValue $ assignmentExpr a
      case assignmentIndex a of
        -- Not an index assignment
        Nothing -> do
          checkType box (vboxType var) aVarName
          setVariable aVar box
        -- Assigning to an index
        Just idx ->
          -- Language is 1-indexed
          let idx' = idx - 1 in
          case vboxValue var of
            -- Variable is an array
            aBox@ArrayBox{} -> do
              let innerType = case arrType aBox of
                    Just (TArray innerTyp) -> Just innerTyp
                    _ -> Nothing
              checkType box innerType aVarName
              let arr = arrVals aBox
              -- make sure extents are correct
              unless (idx' >= 0 && idx' < length arr) $
                throwError $ Errors.invalidIndex aVar idx
              -- stitch the array back together with the new element
              let arr' = take idx' arr ++ [box] ++ drop (idx' + 1) arr
              setVariable aVar (aBox {arrVals = arr'})
            -- Trying to index a non-array
            vbox -> throwError $ Errors.cantIndex aVar vbox
  noReturn
evalStatement i@IfThenElse{} = do
  box <- evalValue $ ifOnVal i
  branch <- boolOrError box
  let stmts = if branch
              then ifThen i
              else ifElse i
  -- TODO should if/then/else be scoped?
  evalStatements stmts

evalStatement s@Switch{} = do
  box <- evalValue $ switchOnVal s
  case_ <- findCase box (switchCases s)
  let body = case case_ of
        Just c -> caseBody c
        Nothing -> switchDefault s
  evalStatements body
  where findCase box1 (case_:cases) = do
          let box2 = boxLiteral $ caseLit case_
          ord <- compareBox box1 box2
          case ord of
            EQ -> return $ Just case_
            _ -> findCase box1 cases
        findCase _ [] = pure Nothing

evalStatement w@While{} = do
  box <- evalValue $ whileVal w
  branch <- boolOrError box
  if branch
    -- recurse only if there's no return value
    then evalStatements (whileBody w) <||> evalStatement w
    else noReturn

evalStatement w@DoWhile{} =
  -- recurse only if there's no return value
  evalStatements (doWhileBody w) <||> do
      box <- evalValue $ doWhileVal w
      branch <- boolOrError box
      if branch
        then evalStatement w
        else noReturn

evalStatement f@For{} = do
  from <- evalValue $ forFrom f
  to <- evalValue $ forTo f
  let var = forVar f
  let typ = forType f
  vals <- boxRange from to
  void $ evalStatement Declaration { declareName = var
                                   , declareVals = []
                                   , declareIsConstant = False
                                   , declareType = Just typ
                                   }
  let doIter val = do
        setVariable var val
        evalStatements $ forBody f
  foldM (\vbox num -> pure vbox <||> doIter num) empty vals
  where
    boxRange from to = case (from, to) of
      -- grumble grumble floating point types
      (NumberBox n1, NumberBox n2) ->
        let n1' = round n1 :: Int
            n2' = round n2 :: Int in
          pure $ map (NumberBox . fromIntegral) (range (n1', n2'))
      (CharacterBox c1, CharacterBox c2) -> pure $ map CharacterBox (range (c1, c2))
      (_, _) -> throwError $ Errors.cantDeduceAnd from to

-- evalStatement Declare{} without the evaling, used by Declare{}, Prompt{},
-- Input{}, and evalMethod
declareVariable :: Evaluator m => Variable -> ValueBox -> Bool -> Maybe Type -> m ()
declareVariable var box isConstant typ = do
  checkType box typ (vName var)
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
               VArrayLookup { vaVariable = varName
                            , vaIndex = i
                            } -> do
                 var <- lookupIdentifier varName
                 case var of
                   ArrayBox { arrVals = arr} ->
                     if length arr > (i - 1)
                     then return $ arr !! (i - 1)
                     else throwError $ Errors.invalidIndex varName i
                   _ -> throwError $ Errors.cantIndex varName var
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
               VMethodCall { vMethodName = name
                           , vMethodArgs = args
                           } -> do
                 argBoxes <- mapM evalValue args
                 m <- gets methods
                 case Map.lookup name m of
                   Just f -> evalMethod f argBoxes
                   Nothing -> throwError $ Errors.noSuchMethod name
               VConcat val -> StringBox <$> evalConcat val

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

evalConcat :: (Evaluator m) => Concat -> m T.Text
evalConcat con = case con of
                 (CLeaf lit) -> return $ toText lit
                 (CValue lit val c') -> do
                   let t1 = toText lit
                   t2 <- printableLiteral <$> evalValue val
                   t3 <- evalConcat c'
                   return $ T.concat [t1, t2, t3]
  where toText lit = case lit of
                       (CharacterLiteral c) -> T.singleton c
                       (StringLiteral t) -> t
                       -- if we get this error, the AST is invalid, so bail
                       _ -> error $ "got unexpected literal" ++ show lit



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
  where
    lookupVariable Variable {vName = varName } = do
      mv <- gets $ Map.lookup varName . variables
      return $ vboxValue <$> mv
    lookupMethod Variable {vName = varName} = do
      mf <- gets (Map.lookup varName . methods)
      case mf of
        Just f -> Just <$> evalMethod f []
        Nothing -> return Nothing

noReturn :: Monad m => m (Maybe a)
noReturn = pure Nothing

-- Short-circuit evaluation if we already have a value
(<||>) :: (Monad m) => m (Maybe b) -> m (Maybe b) -> m (Maybe b)
w1 <||> w2 = do
  m1 <- w1
  case m1 of
    Just{} -> pure m1
    Nothing -> w2

makeIncrDecr :: Variable -> BinaryOperator -> Statement
makeIncrDecr var op =
  Assignment { assignmentName  = var
             , assignmentIndex = Nothing
             , assignmentExpr  =
               VBinaryOperation { vBinArg1 = VVariable var
                                , vBinOpr = op
                                , vBinArg2 = VLiteral (NumberLiteral 1)
                                }

             }
