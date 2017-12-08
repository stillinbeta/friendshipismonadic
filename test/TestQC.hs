{-# LANGUAGE TemplateHaskell #-}

module Main where

import Language.Fim
import Language.Fim.Types
import Language.Fim.Types.Print

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Control.Monad (unless)
import Control.Applicative ((<|>))
import qualified System.Exit
import qualified Data.Text as T

main :: IO ()
main = do
  passed <- checkParallel $$discover
  unless passed System.Exit.exitFailure

prop_parse :: Property
prop_parse = property $ do
  cls <- forAllWith (T.unpack . prettyPrint) genClass
  parse (prettyPrint cls) === Right [cls]

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
genName = Gen.text (Range.linear 1 100) $ Gen.filter (not . isPunctuation) Gen.unicode

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
genStatement = Gen.choice [genOutput]

genOutput :: Gen Statement
genOutput = do
  verb <- Gen.element [Sang, Wrote, Said, Thought]
  value <- genValue
  terminator <- genTerminator
  return Output { outputVerb = verb
                , outputValue = value
                , outputTerminator = terminator}

genValue :: Gen Value
genValue = Gen.choice [VLiteral <$> genLiteral]


genLiteral :: Gen Literal
genLiteral = Gen.choice [genStringLiteral, pure Null ]

genStringLiteral :: Gen Literal
genStringLiteral = do
  quote <- Gen.element [SimpleQuote, FancyQuote]
  -- We don't have escaping
  let filterFunc = case quote of
                     SimpleQuote -> (/='"')
                     FancyQuote  -> (/='”')
  val <- Gen.text (Range.linear 0 250) $ Gen.filter filterFunc Gen.unicode
  return StringLiteral { slValue = val, slWrap = quote}
