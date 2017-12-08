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
  passed <- checkParallel $$(discover)
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
  punctuation <- Gen.element [FullStop, Comma, QuestionMark, Exclamation]
  return Identifier { idName = name, idTerminator = punctuation }

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
  return Function { functionName = name
                  , isMain = today
                  , functionBody = []
                  }
