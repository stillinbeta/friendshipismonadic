{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Main where

import qualified Language.Fim as Fim
import Language.Fim.Types

import qualified Data.Text as T
import NeatInterpolation (text)
import Test.Hspec
import System.IO (stderr)

main :: IO ()
main = hspec $ do
  describe "parser" $ do
    -- basic checks to make sure the parser is behaving. Most complex error will
    -- be caught by SpecHedgehog.hs
    it "should lex a simple, empty class" $ do
      let program = [text|Dear Princess Celestia: Hello World!
                          Your faithful student, Twilight Sparkle.|]

      let expected = Class "Hello World"
                           Celestia []
      Fim.parse program `shouldBe` Right expected
    it "should lex a class with one empty function" $ do
      let program = [text|Dear Princess Celestia: Hello World!

                          Today I learned something simple.
                          That's all about something simple!

                          Your faithful student, Twilight Sparkle.|]
      let expected = Class "Hello World" Celestia [
            Function "something simple" True [] Nothing []
            ]
      Fim.parse program `shouldBe` Right expected
    it "should lex a simple hello world" $ do
      let program = [text|Dear Princess Celestia: Hello World!

                          Today I learned something simple.
                          I said “Hello, World!”!
                          That's all about something simple!

                          Your faithful student, Twilight Sparkle.
                         |]
      let expected = Class "Hello World" Celestia [
            Function "something simple" True [
                Output (
                    VLiteral (StringLiteral "Hello, World!")
                    )
                ] Nothing []
            ]
      Fim.parse program `shouldBe` Right expected
  describe "interpreter" $ do
    describe "input and output" $ do
      it "should output Hello Equestria" $ do
        let program = wrapMethod "I thought “Hello, Equestria!”!\n"
        Fim.run program "" `shouldOutput` "Hello, Equestria!\n"
      it "should throw errors for unknown variables" $ do
        let program = wrapMethod "I sang Hello Equestria!\n"
        Fim.run program "" `shouldError` "Undefined variable <Hello Equestria>"
      it "should output from variables" $ do
        let program = wrapMethod
              [text|Did you know that my greeting is the phrase “Hello, Equestria!”?
                  I sang my greeting!
                  |]
        Fim.run program "" `shouldOutput` "Hello, Equestria!\n"
      it "should accept number input" $ do
        let program = wrapMethod [text|I asked Spike's age.
                                           I said Spike's age plus 1!
                                           |]
        Fim.run program "12" `shouldOutput` "13\n"
      it "should accept boolean input" $ do
        let program = wrapMethod [text|I heard Something.
                                           I said not Something!
                                           |]
        Fim.run program "yes" `shouldOutput` "false\n"
      it "should accept string input" $ do
        let program = wrapMethod [text|I heard Something.
                                           I said Something!
                                           |]
        Fim.run program "Hello, Equestria!" `shouldOutput` "Hello, Equestria!\n"
      it "should error on incorrect input" $ do
        let program = wrapMethod [text|I asked Spike the next number.
                                           |]
        Fim.run program "Applejack" `shouldError`
          "Can't assign string to variable <Spike> of type number"
      it "should accept string input" $ do
        let program = wrapMethod [text|I asked Fluttershy: "What's all the fuss about?"?
                                           I thought Fluttershy.
                                           |]
        Fim.run program "If that's okay" `shouldOutput` "What's all the fuss about?\nIf that's okay\n"
    describe "assigning to variables" $ do
      it "should output from redefined variables" $ do
        let program = wrapMethod
              [text|Did you know that My greeting is a phrase?
                    My greeting is now “Hello, Equestria!”.
                    I said My greeting.
                  |]
        Fim.run program "" `shouldOutput` "Hello, Equestria!\n"
      it "should error on assigning to constants" $ do
        let program = wrapMethod
              [text|Did you know that Applejack is always the number 17?
                    Applejack becomes 18!
                   |]
        Fim.run program "" `shouldError` "Can't assign to constant <Applejack>"
      it "should error on assigning to undefined variables" $ do
        let program = wrapMethod [text|Fluttershy is now 12.|]
        Fim.run program "" `shouldError` "Undefined variable <Fluttershy>"
      it "should error when assigning mixed types" $ do
        let program = wrapMethod
              [text|Did you know that Applejack is the number 17?
                    Applejack becomes "A string"!
                   |]
        Fim.run program "" `shouldError`
          "Can't assign string to variable <Applejack> of type number"
      it "should transfer types across variable declarations" $ do
        let program = wrapMethod
              [text|Did you know that Applejack is the letter 'a'?
                    Did you know that Mare Do Well is Applejack?
                    Mare Do Well is now "Pinkie Pie"!
                   |]
        Fim.run program "" `shouldError`
          "Can't assign string to variable <Mare Do Well> of type character"
    describe "literals" $ do
      it "should support nothing in literals" $ do
        let program = wrapMethod [text|Did you know that Blueblood is the phrase nothing?
                                            I said Blueblood!
                                           |]
        Fim.run program "" `shouldOutput` "nothing\n"
      it "should support boolean literals" $ do
        let program =
              wrapMethod [text|Did you know that Rainbow Dash being straight is the argument no?
                                    I said Rainbow Dash being straight.
                                   |]
        Fim.run program "" `shouldOutput` "false\n"
      it "should support character literals" $ do
        let program = wrapMethod [text|Did you know that my favourite is the letter 'T'?
                                            I sang my favourite!
                                           |]
        Fim.run program "" `shouldOutput` "T\n"
    describe "arithmetic" $ do
      it "should support addition" $ do
        let program = wrapMethod [text|I said 2 and 3 plus 4.25.|]
        Fim.run program "" `shouldOutput` "9.25\n"
      it "should support subtraction" $ do
        let program = wrapMethod [text|I said subtract 19 from 2?|]
        Fim.run program "" `shouldOutput` "17\n"
      it "should support multiplication" $ do
        let program = wrapMethod [text|I said 17 times 83:|]
        Fim.run program "" `shouldOutput` "1411\n"
      it "should support division" $ do
        let program = wrapMethod [text|I said divide 81 by 9!|]
        Fim.run program "" `shouldOutput` "9\n"
    describe "comparisons" $ do
      it "should support equal to" $ do
        let program = wrapMethod [text|I sang "Applejack" is "Bad"!|]
        Fim.run program "" `shouldOutput` "false\n"
      it "should support not equal to" $ do
        let program = wrapMethod [text|I said "blueblood" isn't "good"!|]
        Fim.run program "" `shouldOutput` "true\n"
      it "should support greater than to" $ do
        let program = wrapMethod [text|I said 'd' is more than 'a'?|]
        Fim.run program "" `shouldOutput` "true\n"
      it "should support greater than or equal to" $ do
        let program = wrapMethod [text|I thought 'c' isn't less than 'a':|]
        Fim.run program "" `shouldOutput` "true\n"
      it "should support greater than to" $ do
        let program = wrapMethod [text|I said 'd' is less than 'a'?|]
        Fim.run program "" `shouldOutput` "false\n"
      it "should support greater than or equal to" $ do
        let program = wrapMethod [text|I thought 17 is no more than 18:|]
        Fim.run program "" `shouldOutput` "true\n"
      it "should support heterogenus comparisons" $ do
        let program = wrapMethod [text|I thought 't' is "t"!|]
        Fim.run program "" `shouldOutput` "true\n"
        let program = wrapMethod [text|I thought 19 isn't "a number"?|]
        Fim.run program "" `shouldOutput` "true\n"
    describe "boolean conjuctions" $ do
      it "should support and" $ do
        let program = wrapMethod [text|I thought true and false.|]
        Fim.run program "" `shouldOutput` "false\n"
      it "should support or" $ do
        let program = wrapMethod [text|I thought true or 16 is greater than 17.|]
        Fim.run program "" `shouldOutput` "true\n"
      it "should support xor" $ do
        let program = wrapMethod [text|I thought either true or true.|]
        Fim.run program "" `shouldOutput` "false\n"
      it "should support not" $ do
        let program = wrapMethod [text|I thought it's not the case that 19 is less than 18?|]
        Fim.run program "" `shouldOutput` "true\n"
    describe "conditional logic" $ do
      it "should error on non-booleans" $ do
        let program = wrapMethod [text|When "Chimicherrychanga":
                                            I sang "Pinkie Pie!"!
                                            That's what I would do.
                                           |]
        Fim.run program "" `shouldError` "Expected string to be of type argument"
      it "should branch" $ do
        let program = wrapMethod [text|When 18 isn't less than 10:
                                            I said "I'm pancake"!
                                            That's what I would do.
                                           |]
        Fim.run program "" `shouldOutput` "I'm pancake\n"
      it "should support else" $ do
        let program = wrapMethod [text|When not true:
                                            I said "I'm pancake"!
                                            Otherwise:
                                            I said "I mean... Awake".
                                            That's what I would do.
                                           |]
        Fim.run program "" `shouldOutput` "I mean... Awake\n"
    describe "loops" $ do
      it "should run through loops" $ do
        let program = wrapMethod [text|Did you know that my book count is the number 3?
                                      As long as my book count isn't 0:
                                      I sang my book count!
                                      my book count is now my book count minus 1.
                                      That's what I did.
                                      |]
        Fim.run program "" `shouldOutput` "3\n2\n1\n"
      it "should evaluate do while loops afterwards" $ do
        let program =
              wrapMethod [text|Did you know that Applejack's apple harvest is the number 0?
                                    Here's what I did:
                                    Applejack's apple harvest is now Applejack's apple harvest plus 1.
                                    I said Applejack's apple harvest.
                                    I did this as long as Applejack's apple harvest is less than 3.
                                   |]
        Fim.run program "" `shouldOutput` "1\n2\n3\n"
      it "should support for loops" $ do
        let program =
              wrapMethod [text|For every letter Letter from 'a' to 'z':
                                    I said Letter!
                                    That's what I did.
                                   |]
        Fim.run program "" `shouldOutput` T.pack (concatMap (:"\n") ['a'..'z'])
      it "should no allow loops over strings or booleans" $ do
        let program =
                wrapMethod [text|For every argument Argument from yes to no:
                                      I said Argument.
                                      That's what I did.
                                    |]
        Fim.run program "" `shouldError`
          "Can only iterate over numbers and characters, instead got argument and argument"
        let program2 =
                wrapMethod [text|For every string Name from "Applejack" to "Twilight":
                                      I said Name.
                                      That's what I did.
                                    |]
        Fim.run program "" `shouldError`
          "Can only iterate over numbers and characters, instead got argument and argument"
    describe "calling methods" $ do
      it "should evaluate other methods" $ do
        let program = wrapClass [text|Today I learned something cool.
                                      I would something neat!
                                      That's all about something cool!

                                      I learned something neat:
                                      I sang "Hello, Equestria!".
                                      That's all about something neat!
                                     |]
        Fim.run program "" `shouldOutput` "Hello, Equestria!\n"
      it "should pass arguments to functions" $ do
        let program = wrapClass
              [text|Today I learned something cool.
               I remembered something neat using 2 and 3!
               That's all about something cool!

               I learned something neat using the number x and the number y:
               I said x plus y.
               That's all about something neat!
               |]
        Fim.run program "" `shouldOutput` "5\n"
      it "should error on incorrect number of arguments" $ do
          let program = wrapClass
                [text|Today I learned something cool.
                I remembered something neat using 2 and 3!
                That's all about something cool!

                I learned something neat using the number x:
                I said x!
                That's all about something neat!
                |]
          Fim.run program "" `shouldError`
            "Expected something neat to be called with 1 arguments, but called with 2 arguments"
      it "should error or incorrect type of arguments" $ do
          let program = wrapClass
                [text|Today I learned something cool.
                I remembered something neat using 2!
                That's all about something cool!

                I learned something neat using the letter c:
                I said c!
                That's all about something neat!
                |]
          Fim.run program "" `shouldError`
            "Method something neat argument c has type character but got type number"
      it "should consider methods over variables when namespaces collide." $ do
        let program = wrapClass
              [text|Today I learned something cool.
                    Did you know that something neat is the phrase "Fluttershy"?
                    I said something neat.
                    That's all about something cool!

                    I learned something neat with a phrase:
                    Then you get "Soarin's Apple Pie".
                    That's all about something neat.
                    |]
        Fim.run program "" `shouldOutput` "Soarin's Apple Pie\n"
      describe "return values" $ do
        it "should be produced by methods " $ do
            let program = wrapClass
                  [text|Today I learned something cool.
                  Did you know that my answer is something neat using 7 and 8?
                  I said my answer!
                  That's all about something cool!

                  I learned something neat with a number using the number x and the number y:
                  Then you get x plus y!
                  That's all about something neat!
                  |]
            Fim.run program "" `shouldOutput` "15\n"
        it "should interrupt execution" $ do
          let program = wrapMethod [text|I said "Hello Applejack!".
                                         Then you get nothing.
                                         I said "Hello Spike!".
                                        |]
          Fim.run program "" `shouldOutput` "Hello Applejack!\n"
        it "should break out of loops" $ do
          let program = wrapMethod [text|Did you know that my book count is the number 3?
                                         As long as my book count isn't 0:
                                         I sang my book count!
                                         my book count is now my book count minus 1.
                                         Then you get nothing.
                                         That's what I did.

                                        |]
          Fim.run program "" `shouldOutput` "3\n"
        it "should work from inside if statements" $ do
          let program = wrapClass
                [text|Today I learned something cool.
                      I said something neat using yes.
                      That's all about something cool!

                      I learned something neat with a phrase using the argument arg:
                      If arg: Then you get "you're right!"!
                      Otherwise: Then you get "you're wrong.".
                      That's what I would do.
                      That's all about something neat.
                    |]
          Fim.run program "" `shouldOutput` "you're right!\n"
        it "should return nothing with no return" $ do
          let program = wrapClass
                [text|Today I learned something cool.
                      I said something neat.
                      That's all about something cool!

                      I learned something neat:
                      I remembered "Soarin's Apple Pie".
                      That's all about something neat.
                      |]
          Fim.run program "" `shouldOutput` "nothing\n"
        it "should error when returning the wrong type" $ do
          let program = wrapClass
                [text|Today I learned something cool.
                      I remembered something neat.
                      That's all about something cool!

                      I learned something neat with a phrase:
                      Then you get 17.
                      That's all about something neat.
                      |]
          Fim.run program "" `shouldError`
            "Method something neat has return type string but tried to return number"

shouldOutput :: Either T.Text T.Text -> T.Text -> Expectation
shouldOutput got expected =
  got `shouldBe` Right expected

shouldError :: Either T.Text T.Text -> T.Text -> Expectation
shouldError got expected =
  got `shouldBe` Left expected

wrapClass :: T.Text -> T.Text
wrapClass t = T.concat [ [text|Dear Princess Celestia: Hello World!

                              |]
                       , t
                       , [text|
                             Your faithful student, Beta.
                              |]
                       ]

wrapMethod :: T.Text -> T.Text
wrapMethod t = wrapClass $ T.concat [ [text|Today I learned something simple.
                                           |]
                                    , t
                                    , [text|That's all about something simple!
                                           |]
                                    ]
