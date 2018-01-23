{-# LANGUAGE OverloadedStrings #-}

module Language.Fim.Parser.Gen ( genClass
                               , WithText(..)
                               ) where

import Language.Fim.Types
import Language.Fim.Internal (reservedWordList)

import Control.Applicative ((<|>))
import qualified Data.Text as T
import Data.Char (isDigit)
import Data.Maybe (maybeToList)
import Text.Printf (printf)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

data WithText a = WithText { s :: a
                           , p :: T.Text
                           } deriving (Eq, Show)

wtLift :: (a -> b) -> WithText a -> WithText b
wtLift f (WithText a t) = WithText (f a) t

genClass :: Gen (WithText Class)
genClass = do
  (WithText sName pName) <- genIdentifier
  (WithText sSuper pSuper) <- genSuperClass
  p1 <- genPunctuation
  studentName <- genName
  p2 <- genPunctuation
  body <- Gen.list (Range.linear 1 10) genFunction
  return $ WithText
    Class { className    = sName
          , classSuper   = sSuper
          , classBody    = map s body
          }
    (T.concat $ [ "Dear ", pSuper, ": ", pName, p1, " "]
             ++ map p body
             ++ ["Your faithful student, ", studentName, p2])

genSuperClass :: Gen (WithText Class)
genSuperClass =
  Gen.constant (WithText Celestia "Princess Celestia") <|> genClassByName

genClassByName :: Gen (WithText Class)
genClassByName = do
  name <- genName
  return $ WithText (ClassByName name) name


genFunction :: Gen (WithText Function)
genFunction = do
  name <- genIdentifier
  today <- Gen.bool
  p0 <- genPunctuation
  p1 <- genPunctuation
  body <- genStatements
  ret <- Gen.choice [pure Nothing,  Just <$> genType]
  args <- Gen.list (Range.linear 0 5) genArgument
  let argList = if null args
        then ""
        else " using " `T.append` T.intercalate " and " (map p args)
  returnSig <- case ret of
        Nothing -> pure ""
        Just typ -> do
          verb <- Gen.element ["with", "to get"]
          return $ T.concat [" ", verb, " ", p typ]
  return $ WithText
    Function { functionName = s name
             , functionIsMain = today
             , functionBody = map s body
             , functionReturnType = s <$> ret
             , functionArgs = s <$> args
             }
    (T.concat $ [if today then "Today " else "",
                 "I learned ", p name
                , returnSig
                , argList
                , p0, " "]
             ++ map p body
             ++ ["That's all about ", p name, p1]
    )

genArgument :: Gen (WithText Argument)
genArgument = do
  typ <- genType
  s0 <- genSpace
  idt <- genIdentifier
  let text = T.concat [p typ, s0, p idt]
  return $ WithText Argument { argName = s idt
                             , argType = s typ
                             } text

genStatements :: Gen [WithText Statement]
genStatements = Gen.list (Range.linear 0 20) genStatement

genStatement :: Gen (WithText Statement)
genStatement = Gen.recursive
  Gen.choice
  -- non recursive
  [ genOutput
  , genInput
  , genPrompt
  , genDeclaration
  , genArrayDeclarationNumbered
  , genAssignment
  , genCall
  , genReturn
  , genIncrDecr
  ]
  -- recursive
  [ genIfThenElse
  , genWhile
  , genDoWhile
  , genFor
  , genSwitch
  ]

genOutput :: Gen (WithText Statement)
genOutput = do
  verb <- Gen.element ["sang", "wrote", "said", "thought"]
  val <- genValue
  p0 <- genPunctuation
  return $ WithText
    Output { outputValue = s val }
    (T.concat ["I ", verb, " ", p val, p0])

genInput :: Gen (WithText Statement)
genInput = do
  verb <- Gen.element ["heard", "read", "asked"]
  typ <- Gen.choice [ pure $ WithText Nothing ""
                     , do
                         typ <- genType
                         return $ WithText (Just $ s typ) (" the next " `T.append` p typ)
                     ]
  p0 <- genPunctuation
  var <- genVariable
  let text = T.concat ["I ", verb,  " ", p var, p typ, p0]
  return $ WithText Input { inputName = s var, inputType = s typ } text

genPrompt :: Gen (WithText Statement)
genPrompt = do
  var <- genVariable
  val <- genValue
  p0 <- genPunctuation
  s0 <- genSpace
  let text = T.concat ["I asked ", p var, ": ", p val, p0, s0]
  return $ WithText Prompt { promptName = s var, promptVal = s val} text

genIncrDecr :: Gen (WithText Statement)
genIncrDecr = do
  var <- genVariable
  p0 <- genPunctuation
  s0 <- genSpace
  (opr, str) <- Gen.element [(Increment, "more")
                            ,(Decrement, "less")
                            ]
  let text = T.concat [ p var, " got one ", str, p0, s0]
  return $ WithText (opr $ s var) text

genAssignment :: Gen (WithText Statement)
genAssignment = do
  var <- genVariable
  val <- genValue
  statement <- Gen.element [ "is now"
                           , "are now"
                           , "now like"
                           , "now likes"
                           , "becomes"
                           , "become"
                           ]
  p0 <- genPunctuation
  idx <- Gen.choice [ pure $ WithText Nothing ""
                    , index
                    ]
  return $ WithText
    Assignment { assignmentName = s var
               , assignmentIndex = s idx
               , assignmentExpr = s val
               }
    (T.concat [ p var, p idx, " ", statement, " ", p val, p0])
  where index = do
          i <- Gen.int $ Range.linear 0 100
          return $ WithText (Just i) (" " `T.append` tShow i)

genScalarType :: Gen (WithText Type)
genScalarType = do
  article <- genArticle
  (gen, typ) <- Gen.choice [ pure (genNumberNoun, TNumber)
                           , pure (genStringNoun, TString)
                           , pure (genCharNoun, TCharacter)
                           , pure (genBooleanNoun, TBoolean)
                           ]
  noun <- gen
  return $ WithText typ (T.concat [article, noun])

genArticle :: Gen T.Text
genArticle = Gen.element ["", "the ", "a "]

genType :: Gen (WithText Type)
genType = do
  typ <- genScalarType
  plural <- Gen.element ["s", "es"]
  Gen.element [ typ
              , WithText (TArray $ s typ) (p typ `T.append` plural)
              ]

genDeclarationVerb :: Gen T.Text
genDeclarationVerb =  Gen.element ["is", "was", "has", "had", "like","likes", "liked"]

genDeclaration :: Gen (WithText Statement)
genDeclaration = do
  s0 <- genSpace
  s1 <- genSpace
  s2 <- genSpace
  s3 <- genSpace
  p0 <- genPunctuation
  isConstant <- Gen.bool
  let const = if isConstant
        then WithText True "always "
        else WithText False ""
  var <- genVariable
  verb <- genDeclarationVerb
  vals <- Gen.list (Range.linear 0 10) genShallowValue
  typ <- genType
  let text = T.concat ["Did you know that", s0
                      -- variable
                      , p var, s1
                      -- is/was/etc
                      , verb, s2
                      -- always
                      , p const
                      -- a number, the letter etc
                      , p typ, s3
                      , T.intercalate " and " (p <$> vals),  p0]
  let dec = Declaration { declareName = s var
                        , declareType = Just $ s typ
                        , declareIsConstant = s const
                        , declareVals = s <$> vals
                        }
  return $ WithText dec text

genArrayDeclarationNumbered :: Gen (WithText Statement)
genArrayDeclarationNumbered = do
  plural <- genPlural
  s0 <- genSpace
  s1 <- genSpace
  s2 <- genSpace
  s3 <- genSpace
  s4 <- genSpace
  var <- genVariable
  verb <- genDeclarationVerb
  vals <- Gen.list (Range.linear 1 10) genValue
  typ <- genScalarType
  decs <- genDec var 1 vals
  let text = T.concat ["Did you know that", s0, p var, s1,
                       verb, s2, "many", s3, p typ, plural, "?",
                       s4, decs]
  let dec = Declaration { declareName = s var
                        , declareType = Just . TArray . s $ typ
                        , declareIsConstant = False
                        , declareVals = s <$> vals
                        }
  return $ WithText dec text
  where
    genDec _ _ [] = pure ""
    genDec var i (val:vals) = do
      p0 <- genPunctuation
      s0 <- genSpace
      s1 <- genSpace
      s2 <- genSpace
      s3 <- genSpace
      verb <- genDeclarationVerb
      rest <- genDec var (i+1) vals
      return $ T.concat [ p var, s0, tShow i
                        , s1, verb, s2, p val, p0, s3
                        , rest]

genPlural :: Gen T.Text
genPlural =  Gen.element ["s", "es"]

genNumberNoun :: Gen T.Text
genNumberNoun = pure "number"

genStringNoun :: Gen T.Text
genStringNoun = Gen.element ["word", "phrase", "sentence", "quote", "name"]

genCharNoun :: Gen T.Text
genCharNoun = Gen.element ["letter", "character"]

genBooleanNoun :: Gen T.Text
genBooleanNoun = Gen.element ["argument", "logic"]

genIfThenElse :: Gen (WithText Statement)
genIfThenElse = do
  then_ <- genStatements
  else_ <- genStatements
  val <- genValue
  prefix <- Gen.element ["If", "When"]
  suffix <- Gen.element ["", " then"]
  p1 <- genPunctuation
  s0 <- genSpace
  s1 <- genSpace
  p2 <- genPunctuation
  s2 <- genSpace
  p3 <- genPunctuation
  s3 <- genSpace
  otherwise_ <- Gen.element ["Otherwise", "Or else"]
  let text = T.concat [ prefix, s0, p val, suffix, p1, s1,  sConcat then_
                      , if null else_
                        then ""
                        else T.concat [otherwise_, p2, s2, sConcat else_, s2]
                      , "That's what I would do", p3, s3
                      ]
  let ife = IfThenElse { ifOnVal = s val
                       , ifThen = map s then_
                       , ifElse = map s else_
                       }
  return $ WithText ife text
  where
    sConcat = T.concat . map p

genSwitch :: Gen (WithText Statement)
genSwitch = do
  val <- genValue
  p0 <- genPunctuation
  p1 <- genPunctuation
  s0 <- genSpace
  s1 <- genSpace
  allElse <- Gen.choice [ pure (WithText [] "")
                        , default_
                        ]
  cases <- Gen.list (Range.linear 0 10) genCase
  let text = T.concat ["In regards to ", p val, p0, s0
                      , T.intercalate s1 (p <$> cases)
                      , p allElse
                      , "That's what I did", p1
                      ]
  let switch = Switch { switchOnVal = s val
                      , switchCases = s <$> cases
                      , switchDefault = s allElse
                      }
  return $ WithText switch text
  where default_ = do
          stmts <- genStatements
          p0 <- genPunctuation
          s0 <- genSpace
          return $ WithText (s <$> stmts) $
            T.concat ["If all else fails", p0
            , T.intercalate s0 (p <$> stmts)
            ]

genCase :: Gen (WithText Case)
genCase = do
  lit <- genLiteral
  s0 <- genSpace
  s1 <- genSpace
  p0 <- genPunctuation
  stmts <- genStatements
  suffix <- case s lit of
    -- "yesnd", "nost" don't make sense
    BooleanLiteral{} -> pure ""
    NullLiteral{} -> pure ""
    _ -> Gen.element ["", "st", "nd", "rd", "th"]
  let text = T.concat ["On the ", p lit, suffix, s0, "hoof", p0
                      , T.intercalate s1 (p <$> stmts)
                      ]
  let case_ = Case { caseLit = s lit
                   , caseBody = s <$> stmts
                   }
  return $ WithText case_ text

genWhile :: Gen (WithText Statement)
genWhile = do
  stmts <- genStatements
  s0 <- genSpace
  p1 <- genPunctuation
  s1 <- genSpace
  p2 <- genPunctuation
  s2 <- genSpace
  val <- genValue
  dec <- Gen.element [ "Here's what I did while"
                     , "As long as"
                     ]
  let text = T.concat [ dec, s0, p val, p1, s1
                      , T.concat $ map p stmts
                      , "That's what I did", p2, s2
                      ]
  let typ = While { whileVal = s val
                  , whileBody = map s stmts
                  }
  return $ WithText typ text

genDoWhile :: Gen (WithText Statement)
genDoWhile = do
  stmts <- genStatements
  s0 <- genSpace
  p1 <- genPunctuation
  s1 <- genSpace
  p2 <- genPunctuation
  s2 <- genSpace
  val <- genValue
  while <- Gen.element [ "while"
                       , "as long as"
                       ]
  let text = T.concat [ "Here's what I did ", p1, s0
                      , T.concat $ map p stmts
                      , "I did this ", while, s1, p val, p2, s2
                      ]
  let typ = DoWhile { doWhileBody = map s stmts
                    , doWhileVal  = s val
                    }
  return $ WithText typ text

genFor :: Gen (WithText Statement)
genFor = do
  typ <- Gen.element [ WithText TNumber "number"
                     , WithText TCharacter "letter"
                     , WithText TCharacter "character"
                     ]
  var <- genVariable
  val1 <- genValue
  val2 <- genValue
  s0 <- genSpace
  p1 <- genPunctuation
  s1 <- genSpace
  p2 <- genPunctuation
  s2 <- genSpace
  stmts <- genStatements
  let text = T.concat [ "For every ", p typ, s0, p var
                      , " from ", p val1, " to ", p val2, p1, s1
                      , T.concat $ map p stmts
                      , "That's what I did", p2, s2
                      ]
  let for = For { forVar  = s var
                , forType = s typ
                , forFrom = s val1
                , forTo   = s val2
                , forBody = map s stmts
                }
  return $ WithText for text

genCall :: Gen (WithText Statement)
genCall = do
  verb <- Gen.element ["I would", "I remembered"]
  val <- genValue
  p0 <- genPunctuation
  s0 <- genSpace
  s1 <- genSpace
  let text = T.concat [verb, s0, p val, p0, s1]
  return $ WithText Call {callVal = s val} text

genReturn :: Gen (WithText Statement)
genReturn = do
  val <- genValue
  p0 <- genPunctuation
  s0 <- genSpace
  let text = T.concat ["Then you get ", p val, p0, s0]
  return $ WithText Return {returnVal = s val} text

-----------
-- Value --
-----------
genValue :: Gen (WithText Value)
genValue = genExpr

genShallowValue :: Gen (WithText Value)
genShallowValue  = Gen.choice [ wtLift VLiteral  <$> genLiteral
                              , wtLift VVariable <$> genVariable
                              , genArrayLookup
                              ]

genExpr :: Gen (WithText Value)
genExpr = genValueWithFilter (const True)

genValueWithFilter :: (Value -> Bool) -> Gen (WithText Value)
genValueWithFilter f = Gen.filter (f . s) $ Gen.recursive
  Gen.choice
  [ genShallowValue ]
  -- Parser is left-greedy, so make left value shallow
  [ Gen.subtermM2 genShallowValue (genValueWithFilter f) makeBinaryOperator
  , Gen.subtermM  (genValueWithFilter f) genUnaryOperator
  , genMethodCall
  , genConcat
  ]

genArrayLookup :: Gen (WithText Value)
genArrayLookup = do
  var <- genVariable
  int <- Gen.integral (Range.linear 0 100)
  s0 <- genSpace
  let text = T.concat [p var, s0, tShow int]
  let aLookup = VArrayLookup { vaVariable = s var
                            , vaIndex    = int
                            }
  return $ WithText aLookup text

makeBinaryOperator :: WithText Value -> WithText Value -> Gen (WithText Value)
makeBinaryOperator e1 e2 =
  Gen.choice [ genInfixBinaryOperator e1 e2
             , genPrefixBinaryOperator e1 e2
             ]

genInfixBinaryOperator :: WithText Value -> WithText Value -> Gen (WithText Value)
genInfixBinaryOperator v1 v2 = do
  -- EqualTo (Not ...) parses as NotEqualTo (...)
  (opr, genInfx)  <- case v2 of
                       WithText VUnaryOperation { vUnOpr = Not } _ ->
                         Gen.filter notEqualTo genInfixOpr
                       _ -> genInfixOpr
  infx <- genInfx
  let vbin = VBinaryOperation { vBinArg1 = s v1
                              , vBinOpr  = opr
                              , vBinArg2 = s v2
                              }
  let text =  T.intercalate " " [p v1 , infx, p v2]
  return $ WithText vbin text
  where
    notEqualTo = (/=EqualTo) . fst

genPrefixBinaryOperator :: WithText Value -> WithText Value -> Gen (WithText Value)
genPrefixBinaryOperator v1 v2 = do
  (opr, genPrefix, genConj) <- genPrefixVerbConj
  prefix <- genPrefix
  conj <- genConj
  let vbin = VBinaryOperation { vBinArg1 = s v1
                              , vBinOpr  = opr
                              , vBinArg2 = s v2
                              }
  let text = T.intercalate " " [prefix, p v1, conj, p v2]
  return $ WithText vbin text

genInfixOpr :: Gen (BinaryOperator, Gen T.Text)
genInfixOpr =
  Gen.element [ (Add,      Gen.element [ "plus" , "added to"])
              , (Subtract, Gen.element [ "minus" , "without"])
              , (Multiply, Gen.element [ "times" , "multiplied with"])
              , (Divide,   pure "divided by")

              , (EqualTo,              genComparator)
              , (NotEqualTo,           T.append <$> genComparator <*> genNegation)
              , (LessThan,             T.append <$> genComparator <*> pure " less than")
              , (LessThanOrEqual,      do cmp    <- genComparator
                                          negation <- genNegationWithNo
                                          more   <- genMore
                                          return $ T.concat [cmp, negation, more, " than"])
              , (GreaterThan,          do cmp <- genComparator
                                          more <- genMore
                                          return $ T.concat [cmp, more, " than"])
              , (GreaterThanOrEqual,   do cmp <- genComparator
                                          negation <- genNegationWithNo
                                          return $ T.concat [cmp, negation, " less than"])
              , (And, pure "and")
              , (Or,  pure "or")
              ]
  where
    genNegation = Gen.element [" not", "n't"]
    genNegationWithNo = genNegation <|> pure " no"
    genMore = Gen.element [" more ", " greater "]
    genComparator = Gen.element [ "is" , "was" , "has" , "had"]


genPrefixVerbConj :: Gen (BinaryOperator, Gen T.Text, Gen T.Text)
genPrefixVerbConj =
  Gen.element [ ( Add
                , pure "add"
                , pure "and"
                )
              , ( Subtract
                , Gen.element [ "subtract" , "the difference between"]
                , Gen.element [ "and", "from" ]
                )
              , ( Multiply
                , pure "multiply"
                , pure "and"
                )
              , ( Divide
                , pure "divide"
                , Gen.element [ "and", "by" ]
                )
              , ( Xor
                , pure "either"
                , pure "or"
                )
              ]

genUnaryOperator :: WithText Value -> Gen (WithText Value)
genUnaryOperator val = do
  (opr, genPrefix) <- Gen.element [ (Not, Gen.element [ "not"
                                                      , "it's not the case that"
                                                      ])
                                  ]
  prefix <- genPrefix
  let typ = VUnaryOperation { vUnArg = s val
                           , vUnOpr = opr
                           }
  let text = T.intercalate " " [prefix, p val]
  return $ WithText typ text

genMethodCall :: Gen (WithText Value)
genMethodCall = do
  args <- Gen.list (Range.linear 1 5) genShallowValue
  idt <- genIdentifier
  let text = T.intercalate " " [p idt, "using", T.intercalate " and " (p <$> args)]
  return $ WithText VMethodCall { vMethodName = s idt
                                , vMethodArgs = s <$> args
                                } text

genConcat :: Gen (WithText Value)
genConcat = wtLift VConcat <$> Gen.filter notLeaf genConcat'
  -- Singleton leafs are valid technically, but they're isomorphic to string literals.
  -- The parser always interperts them as the latter, so don't use them here.
  where notLeaf c = case s c of
                      CLeaf{} -> False
                      CValue{} -> True

genConcat' :: Gen (WithText Concat)
genConcat' = Gen.recursive
            Gen.choice
            [ wtLift CLeaf <$> genConcatLiteral]
            [ genCValue ]
  where genCValue = do
          lit <- genConcatLiteral
          val <- genValueWithFilter notCatFilter
          -- if we allow sub-concats, we can't assume the parser will parse them
          -- in the correct order
          recurs <- genConcat'
          let typ = CValue (s lit) (s val) (s recurs)
          let text = T.concat [p lit, p val, p recurs]
          return $ WithText typ text
        genConcatLiteral = Gen.choice [ genStringLiteral
                                      , genConcatLiteral
                                      ]
        notCatFilter f = case f of
                           VConcat{} -> False
                           _ -> True



--------------
-- Literals --
--------------

genLiteral :: Gen (WithText Literal)
genLiteral = Gen.choice [ genCharacterLiteral
                        , genStringLiteral
                        , genNumberLiteral
                        , genBooleanLiteral
                        , genNullLiteral
                        ]

genSingleQuote :: Gen (Char, Char)
genSingleQuote = Gen.element [('\'', '\''), ('‘', '’')]

genDoubleQuote :: Gen (Char, Char)
genDoubleQuote = Gen.element [('"', '"'), ('“', '”')]

genNumberLiteral :: Gen (WithText Literal)
genNumberLiteral = do
  num <- Gen.double (Range.linearFrac 0 1000000)
  return $ WithText
    NumberLiteral {nlValue = num}
    -- printf to avoid exponent
    (T.pack $ printf "%.9G" num)

genCharacterLiteral :: Gen (WithText Literal)
genCharacterLiteral = do
   (open, close) <- genSingleQuote
   char <- Gen.filter (/=close) Gen.unicode
   return $ WithText
     CharacterLiteral { clValue = char
                      }
     (T.pack [open, char, close])

genNullLiteral :: Gen (WithText Literal)
genNullLiteral = pure $ WithText NullLiteral "nothing"

genBooleanLiteral :: Gen (WithText Literal)
genBooleanLiteral = wtLift BooleanLiteral <$>
  Gen.choice [ WithText True  <$> Gen.element [ "yes"
                                              , "true"
                                              , "right"
                                              , "correct"
                                              ]
             , WithText False <$> Gen.element [ "no"
                                              , "false"
                                              , "wrong"
                                              , "incorrect"
                                              ]
             ]

genStringLiteral :: Gen (WithText Literal)
genStringLiteral = do
  (open, close) <- genDoubleQuote
  -- We don't have escaping
  let filterFunc = (/=close)
  val <- Gen.text (Range.linear 0 250) $ Gen.filter filterFunc Gen.unicode
  return $ WithText
    StringLiteral { slValue = val}
    (open `T.cons` val `T.snoc` close)

---------------
-- Variables --
---------------
genName :: Gen T.Text
-- shrinks to Fluttershy, makes errors look a lil nicer :)
genName = pure (T.pack "Fluttershy")
          <|> Gen.text (Range.linear 1 100) (Gen.filter (not . isPunctuation) Gen.unicode)

genIdentifier :: Gen (WithText Identifier)
genIdentifier = do
  article <- genArticle
  idt <- Gen.filter isValidVariable genName
  let t = article `T.append` idt
  return $ WithText t t



genVariable :: Gen (WithText Variable)
genVariable = wtLift Variable <$> genIdentifier

isValidVariable :: T.Text -> Bool
isValidVariable t
  | isDigit t0 = False
  | t0 `elem`  invalidVariableHeadChars = False
  | all ((`T.isInfixOf`t) . T.pack . (' ':)) reservedWordList = False
  | otherwise = True
  where t0 = T.head t

invalidVariableHeadChars :: [Char]
invalidVariableHeadChars = "\"‘“'-"


-----------
-- Utils --
-----------
tShow :: Show a => a -> T.Text
tShow = T.pack . show

genSpace :: Gen T.Text
genSpace = Gen.element [ " "
                       , "\n"
                       , "\t"
                       , "\r\n"
                       ]

genPunctuation :: Gen T.Text
genPunctuation = T.singleton <$> Gen.element ".!?‽…:,"

-- genTerminator :: Gen Terminator
-- genTerminator = Gen.element [FullStop, Comma, QuestionMark, Exclamation]

isPunctuation :: Char -> Bool
isPunctuation char = case char of
                       '.' -> True
                       ',' -> True
                       '!' -> True
                       '?' -> True
                       _   -> False
