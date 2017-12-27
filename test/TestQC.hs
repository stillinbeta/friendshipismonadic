{-# LANGUAGE TemplateHaskell #-}

module Main where

import Language.Fim
import Language.Fim.Parser.Gen

import Hedgehog

import Control.Monad (unless)
import Text.Show.Pretty
import qualified Data.Text as T
import qualified System.Exit

niceShow :: Show a => WithText a -> String
niceShow w = "AST:\n" ++ ppShow (s w) ++ "\nLetter: \n" ++ T.unpack (p w)

main :: IO ()
main = do
  passed <- checkParallel $$discover
  unless passed System.Exit.exitFailure

prop_parse :: Property
prop_parse = property $ do
  cls <- forAllWith niceShow genClass
  parse (p cls) === Right (s cls)
