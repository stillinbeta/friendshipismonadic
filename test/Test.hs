{-# LANGUAGE QuasiQuotes #-}

module Main where

import qualified Fim

import Data.Text (unpack)
import Control.Monad.Trans.Writer.Lazy (execWriter)
import NeatInterpolation (text)
import Test.Hspec

main :: IO ()
main = hspec $
  describe "fim interpreter" $
    it "should run hello world" $ do
      let program = [text|Dear Princess Celestia: Hello World!

                          Today I learned something simple.
                          I said “Hello, World!”!
                          That's all about something simple!

                          Your faithful student, Twilight Sparkle.
                         |]
      execWriter (Fim.run $ unpack program) `shouldBe` ["Hello, World!\n"]
