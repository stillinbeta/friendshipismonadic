{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Main where

import qualified Language.Fim as Fim
import Language.Fim.Types

import qualified Data.Text as T
import NeatInterpolation (text)
import System.IO.Silently (capture, hCapture)
import Test.Hspec
import System.IO (stderr)

main :: IO ()
main = hspec $ do
  describe "parser" $ do
    it "should lex a simple, empty class" $ do
      let program = [text|Dear Princess Celestia: Hello World!
                          Your faithful student, Twilight Sparkle.|]

      let expected = Class (Identifier "Hello World")
                           Celestia []
      Fim.parse program `shouldBe` Right expected
    it "should lex a class with one empty function" $ do
      let program = [text|Dear Princess Celestia: Hello World!

                          Today I learned something simple.
                          That's all about something simple!

                          Your faithful student, Twilight Sparkle.|]
      let expected = Class (Identifier "Hello World" ) Celestia [
            Function (Identifier "something simple") True []
            ]
      Fim.parse program `shouldBe` Right expected
    it "should lex a simple hello world" $ do
      let program = [text|Dear Princess Celestia: Hello World!

                          Today I learned something simple.
                          I said “Hello, World!”!
                          That's all about something simple!

                          Your faithful student, Twilight Sparkle.
                         |]
      let expected = Class (Identifier "Hello World") Celestia [
            Function (Identifier "something simple") True [
                Output (
                    VLiteral (StringLiteral "Hello, World!")
                    )
                ]
            ]
      Fim.parse program `shouldBe` Right expected
  describe "interpreter" $ do
    it "should output Hello Equestria" $ do
      let program = wrapBoilerplate "I thought “Hello, Equestria!”!\n"
      capture (Fim.run program) `shouldReturn` ("Hello, Equestria!\n", Nothing)
    it "should throw errors for unknown variables" $ do
      let program = wrapBoilerplate "I sang Hello Equestria!\n"
      Fim.run program `shouldOutputToStderr` "undefined variable Hello Equestria\n"
    it "should output from variables" $ do
      let program = wrapBoilerplate
            [text|Did you know that my greeting is the phrase “Hello, Equestria!”?
                 I sang my greeting!
                 |]
      Fim.run program `shouldOutput` "Hello, Equestria!\n"
    describe "assigning to variables" $ do
      it "should output from redefined variables" $ do
        let program = wrapBoilerplate
              [text|Did you know that My greeting is a phrase?
                    My greeting is now “Hello, Equestria!”.
                    I said My greeting.
                  |]
        Fim.run program `shouldOutput` "Hello, Equestria!\n"
      it "should error on assigning to constants" $ do
        let program = wrapBoilerplate
              [text|Did you know that Applejack is always the number 17?
                    Applejack becomes 18!
                   |]
        Fim.run program `shouldOutputToStderr` "can't redefine constant Applejack\n"
      it "should error on assigning to undefined variables" $ do
        let program = wrapBoilerplate [text|Fluttershy is now 12.|]
        Fim.run program `shouldOutputToStderr` "Undefined variable Fluttershy\n"
      it "should error when assigning mixed types" $ do
        let program = wrapBoilerplate
              [text|Did you know that Applejack is the number 17?
                    Applejack becomes "A string"!
                   |]
        Fim.run program `shouldOutputToStderr`
          "Can't assign string to variable Applejack of type number\n"
      it "should transfer types across variable declarations" $ do
        let program = wrapBoilerplate
              [text|Did you know that Applejack is the letter 'a'?
                    Did you know that Mare Do Well is Applejack?
                    Mare Do Well is now "Pinkie Pie"!
                   |]
        Fim.run program `shouldOutputToStderr`
          "Can't assign string to variable Mare Do Well of type character\n"


shouldOutput :: IO (Maybe String) -> String -> Expectation
shouldOutput io str =
  capture io `shouldReturn` (str, Nothing)

shouldOutputToStderr :: IO (Maybe String) -> String -> Expectation
shouldOutputToStderr io str =
  hCapture [stderr] io `shouldReturn` (str, Nothing)


wrapBoilerplate :: T.Text -> T.Text
wrapBoilerplate t = T.concat [ [text|Dear Princess Celestia: Hello World!

                                     Today I learned something simple.
                                     |]
                             , t
                             , [text|That's all about something simple!

                                     Your faithful student, Twilight Sparkle.
                                     |]
                             ]
