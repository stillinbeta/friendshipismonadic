{-# LANGUAGE OverloadedStrings #-}

module Language.Fim.Parser.Gen ( genClass
                               , WithText(..)
                               ) where

import Language.Fim.Types
import Language.Fim.Internal (reservedWordList)

import Control.Applicative ((<|>))
import qualified Data.Text as T
import Data.Char (isDigit)
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
  body <- Gen.list (Range.linear 1 100) genFunction
  return $ WithText
    Class { className    = sName
          , classSuper   = sSuper
          , classBody    = map s body
          }
    (T.concat $ [ "Dear ", pSuper, ": ", pName, p1, "\n"]
             ++ map p body
             ++ ["Your faithful student, ", studentName, p2, "\n"])

genSuperClass :: Gen (WithText Class)
genSuperClass =
  Gen.constant (WithText Celestia "Princess Celestia") <|> genClassByName

genClassByName :: Gen (WithText Class)
genClassByName = do
  name <- genName
  return $ WithText (ClassByName name) name

genPunctuation :: Gen T.Text
genPunctuation = T.singleton <$> Gen.element ".!?‽…:"

genIdentifier :: Gen (WithText Identifier)
genIdentifier = do
   name <- genName
   return $ WithText (Identifier name) name

-- genTerminator :: Gen Terminator
-- genTerminator = Gen.element [FullStop, Comma, QuestionMark, Exclamation]

genName :: Gen T.Text
-- shrinks to Fluttershy, makes errors look a lil nicer :)
genName = pure (T.pack "Fluttershy")
          <|> Gen.text (Range.linear 1 100) (Gen.filter (not . isPunctuation) Gen.unicode)

isPunctuation :: Char -> Bool
isPunctuation char = case char of
                       '.' -> True
                       ',' -> True
                       '!' -> True
                       '?' -> True
                       _   -> False

genFunction :: Gen (WithText Function)
genFunction = do
  name <- genIdentifier
  today <- Gen.bool_
  p0 <- genPunctuation
  p1 <- genPunctuation
  body <- Gen.list (Range.linear 0 100) genStatement
  return $ WithText
    Function { functionName = s name
             , isMain = today
             , functionBody = map s body
             }
    (T.concat $ [if today then "Today " else "", "I learned ", p name, p0, "\n"]
             ++ map p body
             ++ ["That's all about ", p name, p1, "\n"]
    )

genStatement :: Gen (WithText Statement)
genStatement = Gen.choice [ genOutput
                          , genDeclaration
                          , genAssignment
                          ]

genOutput :: Gen (WithText Statement)
genOutput = do
  verb <- Gen.element ["sang", "wrote", "said", "thought"]
  val <- genValue
  p0 <- genPunctuation
  return $ WithText
    Output { outputValue = s val }
    (T.concat ["I ", verb, " ", p val, p0, "\n"])

genDeclaration :: Gen (WithText Statement)
genDeclaration = do
  verb <- Gen.element ["is", "was", "has", "had", "like","likes", "liked"]
  name <- genVariable
  isConstant <- Gen.bool
  (val, typ) <- Gen.choice [ genDeclarationNothingTyped
                           , genDeclarationLiteralTyped
                           , genDeclarationVariable
                           ]
  return $ WithText
    Declaration { declareName = s name
                , declareVal = s val
                , declareIsConstant = isConstant
                , declareType = typ
                }
    (T.concat [ "Did you know that ", p name, " ", verb,
               if isConstant then " always " else " ",
               p val, "?\n"
               ]
    )

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
  return $ WithText
    Assignment { assignmentName = s var
               , assignmentExpr = s val
               }
    (T.concat [ p var, " ", statement, " ", p val, p0, "\n"])

genDeclarationNothingTyped :: Gen (WithText (Maybe Value), Maybe Type)
genDeclarationNothingTyped = do
  article <- Gen.element ["", "the ", "a "]
  (gen, typ) <- Gen.choice [ pure (genNumberNoun, TNumber)
                           , pure (genStringNoun, TString)
                           , pure (genCharNoun, TCharacter)
                           ]
  noun <- gen
  return (WithText Nothing $ T.concat [article, noun], Just typ)

genDeclarationLiteralTyped :: Gen (WithText (Maybe Value), Maybe Type)
genDeclarationLiteralTyped = do
  article <- Gen.element ["", "the ", "a "]
  lit <- genLiteral
  let (gen, typ) = case s lit of
            NumberLiteral{} ->    (genNumberNoun, pure TNumber)
            StringLiteral{} ->    (genStringNoun, pure TString)
            CharacterLiteral{} -> (genCharNoun, pure TCharacter)
  noun <- gen
  return (WithText
          (Just VLiteral { vLiteral = s lit})
          (T.concat [article , noun, " ", p lit])
         , typ)

genDeclarationVariable :: Gen (WithText (Maybe Value), Maybe Type)
genDeclarationVariable = do
  var <- genVariable
  return (WithText (Just . VVariable $ s var) (p var), Nothing)

genNumberNoun :: Gen T.Text
genNumberNoun = pure "number"

genStringNoun :: Gen T.Text
genStringNoun = Gen.element ["word", "phrase", "sentence", "quote", "name"]

genCharNoun :: Gen T.Text
genCharNoun = Gen.element ["letter", "character"]


-----------
-- Value --
-----------
genValue :: Gen (WithText Value)
genValue = Gen.choice [ genShallowValue
                      , genExpr
                      ]

genShallowValue :: Gen (WithText Value)
genShallowValue  = Gen.choice [ wtLift VLiteral  <$> genLiteral
                              , wtLift VVariable <$> genVariable
                              ]

genExpr :: Gen (WithText Value)
genExpr = Gen.recursive
  Gen.choice
  [ genShallowValue ]
  -- Parser is left-greedy
  [ Gen.subtermM2 genShallowValue genExpr makeBinaryOperator]

makeBinaryOperator :: WithText Value -> WithText Value -> Gen (WithText Value)
makeBinaryOperator e1 e2 = do
  opr <- genBinaryOperator
  txt <- Gen.choice [ genInfixBinaryOperator opr e1 e2
                    , genPrefixBinaryOperator opr e1 e2
                    ]
  let binOp = VBinaryOperation { vBinArg1 = s e1
                               , vBinOpr = opr
                               , vBinArg2 = s e2
                               }
  return $ WithText binOp txt

genInfixBinaryOperator :: BinaryOperator -> WithText Value -> WithText Value -> Gen T.Text
genInfixBinaryOperator opr v1 v2 = do
  infixV <- genInfixVerb opr
  return $ T.intercalate " " [p v1 , infixV, p v2]

genPrefixBinaryOperator :: BinaryOperator -> WithText Value -> WithText Value -> Gen T.Text
genPrefixBinaryOperator opr v1 v2 = do
  prefixV <- genPrefixVerb opr
  prefixC <- genPrefixConjuction opr
  return $ T.intercalate " " [prefixV, p v1, prefixC, p v2]

genInfixVerb :: BinaryOperator -> Gen T.Text
genInfixVerb opr = case opr of
  Add      -> Gen.element [ "plus"
                          , "and"
                          , "added to"
                          ]
  Subtract -> Gen.element [ "minus"
                          , "without"
                          ]
  Multiply -> Gen.element [ "times"
                          , "multiplied with"
                          ]
  Divide   -> pure "divided by"

genPrefixVerb :: BinaryOperator -> Gen T.Text
genPrefixVerb opr = case opr of
  Add      -> pure "add"
  Subtract -> Gen.element [ "subtract"
                          , "the difference between"
                          ]
  Multiply -> pure "multiply"
  Divide   -> pure "divide"

genPrefixConjuction :: BinaryOperator -> Gen T.Text
genPrefixConjuction opr = case opr of
  Add      -> pure "and"
  Subtract -> Gen.element [ "and"
                          , "from"
                          ]
  Multiply -> pure "and"
  Divide   -> Gen.element [ "and"
                          , "by"
                          ]

genBinaryOperator :: Gen BinaryOperator
genBinaryOperator = Gen.element [ Add, Subtract, Multiply, Divide ]

--------------
-- Literals --
--------------

genLiteral :: Gen (WithText Literal)
genLiteral = Gen.choice [ genCharacterLiteral
                        , genStringLiteral
                        , genNumberLiteral]

genSingleQuote :: Gen (Char, Char)
genSingleQuote = Gen.element [('\'', '\''), ('‘', '’')]

genDoubleQuote :: Gen (Char, Char)
genDoubleQuote = Gen.element [('"', '"'), ('“', '”')]

genNumberLiteral :: Gen (WithText Literal)
genNumberLiteral = do
  num <- Gen.double (Range.linearFrac (-1000000) 1000000)
  return $ WithText
    NumberLiteral {nlValue = num}
    -- printf to avoid exponent
    (T.pack $ printf "%-.9G" num)

genCharacterLiteral :: Gen (WithText Literal)
genCharacterLiteral = do
   (open, close) <- genSingleQuote
   char <- Gen.filter (/=close) Gen.unicode
   return $ WithText
     CharacterLiteral { clValue = char
                      }
     (T.pack [open, char, close])

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

genVariable :: Gen (WithText Variable)
genVariable = do
  name <- Gen.filter isValidVariable genName
  return $ WithText Variable { vName = name } name

isValidVariable :: T.Text -> Bool
isValidVariable t
  | isDigit t0 = False
  | t0 `elem`  invalidVariableHeadChars = False
  | all ((`T.isInfixOf`t) . T.pack . (' ':)) reservedWordList = False
  | otherwise = True
  where t0 = T.head t

invalidVariableHeadChars :: [Char]
invalidVariableHeadChars = "\"‘“'-"
