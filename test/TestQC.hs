{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Main where

import Language.Fim
import Language.Fim.Types
import Language.Fim.Parser.Tokens (reservedWordList)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Text.Printf (printf)
import Control.Monad (unless)
import Control.Applicative ((<|>))
import Data.Char (isDigit)
import qualified Data.Text as T
import Text.Show.Pretty (ppShow)
import qualified System.Exit


data WithText a = WithText { s :: a
                           , p :: T.Text
                           } deriving (Eq, Show)

niceShow :: Show a => WithText a -> String
niceShow w = "AST:\n" ++ ppShow (s w) ++ "\nLetter: \n" ++ T.unpack (p w)

main :: IO ()
main = do
  passed <- checkParallel $$discover
  unless passed System.Exit.exitFailure

prop_parse :: Property
prop_parse = property $ do
  cls <- forAllWith niceShow genClass
  -- cls <- forAll genClass
  parse (p cls) === Right (s cls)

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
genPunctuation = Gen.element [".", ",", "!", "?"]

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
  expr <- genExpr
  p0 <- genPunctuation
  return $ WithText
    Output { outputExpr = s expr }
    (T.concat ["I ", verb, " ", p expr, p0, "\n"])

genDeclaration :: Gen (WithText Statement)
genDeclaration = do
  verb <- Gen.element ["is", "was", "has", "had", "like","likes", "liked"]
  name <- genVariable
  isConstant <- Gen.bool
  (expr, typ) <- Gen.choice [ genDeclarationNothingTyped
                           , genDeclarationLiteralTyped
                           , genDeclarationVariable
                           ]
  return $ WithText
    Declaration { declareName = s name
                , declareExpr = s expr
                , declareIsConsnant = isConstant
                , declareType = typ
                }
    (T.concat [ "Did you know that ", p name, " ", verb,
               if isConstant then " always " else " ",
               p expr, "?\n"
               ]
    )

genAssignment :: Gen (WithText Statement)
genAssignment = do
  var <- genVariable
  expr <- genExpr
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
               , assignmentExpr = s expr
               }
    (T.concat [ p var, " ", statement, " ", p expr, p0, "\n"])

genDeclarationNothingTyped :: Gen (WithText (Maybe Expression), Maybe Type)
genDeclarationNothingTyped = do
  article <- Gen.element ["", "the ", "a "]
  (gen, typ) <- Gen.choice [ pure (genNumberNoun, TNumber)
                           , pure (genStringNoun, TString)
                           , pure (genCharNoun, TCharacter)
                           ]
  noun <- gen
  return (WithText Nothing $ T.concat [article, noun], Just typ)

genDeclarationLiteralTyped :: Gen (WithText (Maybe Expression), Maybe Type)
genDeclarationLiteralTyped = do
  article <- Gen.element ["", "the ", "a "]
  lit <- genLiteral
  let (gen, typ) = case s lit of
            NumberLiteral{} ->    (genNumberNoun, pure TNumber)
            StringLiteral{} ->    (genStringNoun, pure TString)
            CharacterLiteral{} -> (genCharNoun, pure TCharacter)
  noun <- gen
  return (WithText
          (Just ELiteral { eLiteral = s lit})
          (T.concat [article , noun, " ", p lit])
         , typ)

genDeclarationVariable :: Gen (WithText (Maybe Expression), Maybe Type)
genDeclarationVariable = do
  var <- genVariable
  return (WithText (Just . EVariable $ s var) (p var), Nothing)

genNumberNoun :: Gen T.Text
genNumberNoun = pure "number"

genStringNoun :: Gen T.Text
genStringNoun = Gen.element ["word", "phrase", "sentence", "quote", "name"]

genCharNoun :: Gen T.Text
genCharNoun = Gen.element ["letter", "character"]

-- TODO: enforce a/an consistency
genArticle :: Gen T.Text
genArticle = Gen.element ["The", "A", "An"]

genExpr :: Gen (WithText Expression)
genExpr = Gen.choice [ genVLiteral
                      --, VVariable <$> genVariable
                      ]

genVLiteral :: Gen (WithText Expression)
genVLiteral = do
  lit <- genLiteral
  return $ WithText (ELiteral $ s lit) (p lit)

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
