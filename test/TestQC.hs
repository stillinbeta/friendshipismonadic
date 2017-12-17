{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Main where

import Language.Fim
import Language.Fim.Types
import Language.Fim.Parser.Tokens (reservedWordList)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

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
                          ]

genOutput :: Gen (WithText Statement)
genOutput = do
  verb <- Gen.element ["sang", "wrote", "said", "thought"]
  value <- genValue
  p0 <- genPunctuation
  return $ WithText
    Output { outputValue = s value }
    (T.concat ["I ", verb, " ", p value, p0, "\n"])

genDeclaration :: Gen (WithText Statement)
genDeclaration = do
  verb <- Gen.element ["is", "was", "has", "had", "like","likes", "liked"]
  name <- genVariable
  isConstant <- Gen.bool
  val <- Gen.choice [ genDeclarationNothingTyped
                    , genDeclarationLiteralTyped
                    , genDeclarationVariable
                    ]
  p0 <- genPunctuation
  return $ WithText
    Declaration { declareName = s name
                , declareValue = s val
                , declareIsConsnant = isConstant
                }
    (T.concat [ "Did you know that ", p name,
               if isConstant then " always" else "", " ",
               verb, " ", p val, p0, "\n"
               ]
    )

genDeclarationNothingTyped :: Gen (WithText Value)
genDeclarationNothingTyped = do
  article <- Gen.element ["", "the ", "a "]
  noun <- Gen.choice [genNumberNoun, genStringNoun, genCharNoun]
  return $ WithText VNull (T.concat [article, noun])

genDeclarationLiteralTyped :: Gen (WithText Value)
genDeclarationLiteralTyped = do
  article <- Gen.element ["", "the ", "a "]
  lit <- genLiteral
  noun <- case s lit of
            NumberLiteral{} ->    genNumberNoun
            StringLiteral{} ->    genStringNoun
            CharacterLiteral{} -> genCharNoun
  return $ WithText
    VLiteral { vLiteral = s lit}
    (T.concat [article , " ", noun, " ", p lit])

genDeclarationVariable :: Gen (WithText Value)
genDeclarationVariable = do
  var <- genVariable
  return $ WithText (VVariable $ s var) (p var)

genNumberNoun :: Gen T.Text
genNumberNoun = pure "number"

genStringNoun :: Gen T.Text
genStringNoun = Gen.element ["word", "phrase", "sentence", "quote", "name"]

genCharNoun :: Gen T.Text
genCharNoun = Gen.element ["letter", "character"]

-- TODO: enforce a/an consistency
genArticle :: Gen T.Text
genArticle = Gen.element ["The", "A", "An"]

genValue :: Gen (WithText Value)
genValue = Gen.choice [ genVLiteral
                      --, VVariable <$> genVariable
                      ]

genVLiteral :: Gen (WithText Value)
genVLiteral = do
  lit <- genLiteral
  return $ WithText (VLiteral $ s lit) (p lit)

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
    (T.pack . show $ num)

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
isValidVariable t =
  let t0 = T.head t in
    not (isDigit t0)
    && t0 `notElem` invalidVariableHeadChars
    && not (all ((`T.isInfixOf`t) . T.pack . (' ':)) reservedWordList)

invalidVariableHeadChars :: [Char]
invalidVariableHeadChars = "\"‘“'-"
