{-# LANGUAGE TemplateHaskell #-}

module Main where

import Language.Fim
import Language.Fim.Types
import Language.Fim.Types.Print

import Language.Fim.Parser.Tokens (reservedWordList)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Control.Monad (unless)
import Control.Applicative ((<|>))
import Data.Char (isDigit)
import qualified Data.Text as T
import qualified System.Exit

main :: IO ()
main = do
  passed <- checkParallel $$discover
  unless passed System.Exit.exitFailure

prop_parse :: Property
prop_parse = property $ do
  cls <- forAllWith (T.unpack . prettyPrint) genClass
  -- cls <- forAll genClass
  parse (prettyPrint cls) === Right cls

genClass :: Gen Class
genClass = do
  name <- genIdentifier
  super <- genSuperClass
  student <- genIdentifier
  body <- Gen.list (Range.linear 1 100) genFunction
  return Class { className    = name
               , classSuper   = super
               , classBody    = body
               , classStudent = student
               }

genSuperClass :: Gen Class
genSuperClass =
  Gen.constant Celestia <|> do ClassByName <$> genName

genIdentifier :: Gen Identifier
genIdentifier = do
  name <- genName
  punctuation <- genTerminator
  return Identifier { idName = name, idTerminator = punctuation }

genTerminator :: Gen Terminator
genTerminator = Gen.element [FullStop, Comma, QuestionMark, Exclamation]

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

genFunction :: Gen Function
genFunction = do
  name <- genIdentifier
  today <- Gen.bool_
  body <- Gen.list (Range.linear 0 100) genStatement
  return Function { functionName = name
                  , isMain = today
                  , functionBody = body
                  }

genStatement :: Gen Statement
genStatement = Gen.choice [ genOutput
                          , genDeclaration]

genOutput :: Gen Statement
genOutput = do
  verb <- Gen.element [Sang, Wrote, Said, Thought]
  value <- genValue
  terminator <- genTerminator
  return Output { outputVerb = verb
                , outputValue = value
                , outputTerminator = terminator}

genDeclaration :: Gen Statement
genDeclaration = do
  verb <- Gen.element [Is, Was, Has, Had, Like, Likes, Liked]
  name <- genVariable
  value <- genLiteral
  isConstant <- Gen.bool
  return Declaration { declareVerb = verb
                     , declareName = name
                     , declareValue = value
                     , declareIsConsnant = isConstant
                     }

genValue :: Gen Value
genValue = Gen.choice [ VLiteral <$> genLiteral
                      , VVariable <$> genVariable]

-- Literals --

genLiteral :: Gen Literal
genLiteral = Gen.choice [pure Null
                        , genCharacterLiteral
                        , genStringLiteral
                        , genNumberLiteral]

genNumberLiteral :: Gen Literal
genNumberLiteral = NumberLiteral <$> Gen.double (Range.linearFrac (-1000000) 1000000)

genCharacterLiteral :: Gen Literal
genCharacterLiteral = do
  char <- Gen.unicode
  quote <- genQuote
  return CharacterLiteral { clValue = char
                          , clWrap = quote
                          }

genStringLiteral :: Gen Literal
genStringLiteral = do
  quote <- genQuote
  -- We don't have escaping
  let filterFunc = case quote of
                     SimpleQuote -> (/='"')
                     FancyQuote  -> (/='”')
  val <- Gen.text (Range.linear 0 250) $ Gen.filter filterFunc Gen.unicode
  return StringLiteral { slValue = val, slWrap = quote}

genQuote :: Gen StringQuote
genQuote = Gen.element [SimpleQuote, FancyQuote]

-- Variables --

genVariable :: Gen Variable
genVariable = do
  name <- Gen.filter isValidVariable genName
  return Variable { vName = name }

isValidVariable :: T.Text -> Bool
isValidVariable t =
  let t0 = T.head t in
    (not (isDigit t0))
    && (t0 `notElem` "\"‘“'-")
    && (not $ all ((`T.isInfixOf`t) . T.pack . (' ':)) reservedWordList)
