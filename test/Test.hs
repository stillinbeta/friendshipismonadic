{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Main where

import qualified Language.Fim as Fim
import Language.Fim.Types

import qualified Data.Text as T
import NeatInterpolation (text)
import System.IO.Silently (capture)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "lexer" $ do
    it "should lex a simple, empty class" $ do
      let program = [text|Dear Princess Celestia: Hello World!
                          Your faithful student, Twilight Sparkle.|]

      let expected = Class (Identifier "Hello World" Exclamation)
                           Celestia []
                           (Identifier "Twilight Sparkle" FullStop)
      Fim.parse program `shouldBe` Right [expected]
    it "should lex a class with one empty function" $ do
      let program = [text|Dear Princess Celestia: Hello World!

                          Today I learned something simple.
                          That's all about something simple!

                          Your faithful student, Twilight Sparkle.|]
      let expected = Class (Identifier "Hello World" Exclamation) Celestia [
            Function (Identifier "something simple" FullStop) True []
            ] (Identifier "Twilight Sparkle" FullStop)
      Fim.parse program `shouldBe` Right [expected]
    it "should lex a simple hello world" $ do
      let program = [text|Dear Princess Celestia: Hello World!

                          Today I learned something simple.
                          I said “Hello, World!”!
                          That's all about something simple!

                          Your faithful student, Twilight Sparkle.
                         |]
      let expected = Class (Identifier "Hello World" Exclamation) Celestia [
            Function (Identifier "something simple" FullStop) True [
                Output Said (
                    VLiteral (StringLiteral "Hello, World!" FancyQuote )
                    )
                    Exclamation
                ]
            ] (Identifier "Twilight Sparkle" FullStop)

      Fim.parse program `shouldBe` Right [expected]
  describe "interpreter" $ do
    it "should output Hello Equestria" $ do
      let program = wrapBoilerplate "I thought “Hello, Equestria!”!\n"
      capture (Fim.run program) `shouldReturn` ("Hello, Equestria!\n", Nothing)

wrapBoilerplate :: T.Text -> T.Text
wrapBoilerplate t = T.concat [ [text|Dear Princess Celestia: Hello World!

                                     Today I learned something simple.
                                     |]
                             , t
                             , [text|That's all about something simple!

                                     Your faithful student, Twilight Sparkle.
                                     |]
                             ]
