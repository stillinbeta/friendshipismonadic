{-# LANGUAGE QuasiQuotes #-}

module Main where

import qualified Fim
import qualified Fim.Types as Types

import Data.Text (unpack)
import Control.Monad.Trans.Writer.Lazy (execWriter)
import NeatInterpolation (text)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "lexer" $ do
    it "should lex a simple, empty class" $ do
      let program = [text|Dear Princess Celestia: Hello World!
                          Your faithful student, Twilight Sparkle.|]
      Fim.parse (unpack program) `shouldBe` Right [Types.Class "Hello World" Types.Celestia []]
    it "should lex a class with one empty function" $ do
      let program = [text|Dear Princess Celestia: Hello World!

                          Today I learned something simple.
                          That's all about something simple!

                          Your faithful student, Twilight Sparkle.|]
      Fim.parse (unpack program) `shouldBe` Right [Types.Class
                                                   "Hello World"
                                                   Types.Celestia
                                                  [Types.Function "something simple"]]

  describe "fim interpreter" $
    it "should run hello world" $ do
      let program = [text|Dear Princess Celestia: Hello World!

                          Today I learned something simple.
                          I said “Hello, World!”!
                          That's all about something simple!

                          Your faithful student, Twilight Sparkle.
                         |]
      pending
      execWriter (Fim.run $ unpack program) `shouldBe` ["Hello, World!\n"]
