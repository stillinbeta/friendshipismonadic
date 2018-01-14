{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Main where

import qualified Language.Fim as Fim
import Language.Fim.Types

import qualified Data.Text as T
import NeatInterpolation (text)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "parser" $ do
    -- basic checks to make sure the parser is behaving. Most complex error will
    -- be caught by TestHedgehog.hs
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
      it "should support increments" $ do
        let program = wrapMethod [text|Did you know that Spike's Allowance is the number 17?
                                       Spike's Allowance got one more!
                                       I said Spike's Allowance.
                                      |]
        Fim.run program "" `shouldOutput` "18\n"
      it "should support decrements" $ do
        let program = wrapMethod [text|Did you know that Rarity's mane length is the number 12?
                                       Rarity's mane length got one less!
                                       I said Rarity's mane length.
                                      |]
        Fim.run program "" `shouldOutput` "11\n"
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
    describe "comments" $ do
      it "should work anywhere" $ do
        let program = wrapClass [text|(This is a comment!)
                                     P.P.S. This is also a comment
                                     Today I learned something awesome!
                                     (They work here too!)
                                     P.S. this kind also works!
                                     I said "Rainbow Dash"!
                                     That's all about something awesome!
                                     P.P.P.S. Thanks for reading!
                                     |]
        Fim.run program "" `shouldOutput` "Rainbow Dash\n"
      describe "with parentheses" $
        it "should wrap around newlines" $ do
          let program = wrapMethod [text|I said "I'm stitching Twilight's dress".
                                         (I said "Never stressed!"
                                          I said "Hook and eye, couldn't you just simply die!"
                                         ) I said "The customer is always right"!
                                        |]
          Fim.run program "" `shouldOutput` "I'm stitching Twilight's dress\nThe customer is always right\n"
      describe "with p.s." $ do
        it "should support arbitrary many P.Ses" $ do
          let program = wrapMethod [text|I said "I'm pancake"!
                                         P.S. I was totally not asleep
                                         P.P.S. Yes you were, Twilight
                                         P.P.P.S. No editorialising, Spike!
                                         P.P.P.P.S. Maybe you should write your own letters then!
                                        |]
          Fim.run program "" `shouldOutput` "I'm pancake\n"
        it "should comment out until the end of the newline" $ do
          let program = wrapMethod [text|I said "I am not tardy!". P.S. I totally wasn't!
                                         I said "Everything is fine!"!
                                         |]
          Fim.run program "" `shouldOutput` "I am not tardy!\nEverything is fine!\n"
    describe "conditional logic" $ do
      describe "if statements" $ do
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
      describe "switch statements"  $ do
        it "should switch on numbers" $ do
          let program = wrapMethod [text|In regards to 1 plus 2:
                                         On the 1st hoof:
                                         I said "One"!
                                         On the 2nd hoof:
                                         I said "Two"!
                                         On the 3rd hoof:
                                         I said "Three"!
                                         On the 4th hoof:
                                         I said "Four"!
                                         That's what I did.
                                        |]
          Fim.run program "" `shouldOutput` "Three\n"
        it "should do string comparison" $ do
          let program = wrapMethod [text|In regards to yes:
                                        On the "true" hoof:
                                        I said "That's an aye"!
                                        On the "false" hoof:
                                        I said "That's a neigh"!
                                        That's what I did.
                                        |]
          Fim.run program "" `shouldOutput` "That's an aye\n"
        it "should evaluate defaults" $ do
          let program = wrapMethod [text|In regards to "Twilight Sparkle":
                                         On the "Fluttershy" hoof:
                                         I said "Hi Fluttershy"!
                                         On the "Rarity" hoof:
                                         I said "Hiya Rarity"!
                                         On the "Applejack" hoof:
                                         I said "Howdy Applejack"!
                                         On the "Rainbow Dash" hoof:
                                         I said "Hey Dashie"!
                                         If all else fails:
                                         I sang "Time to throw a party"!
                                         That's what I did.
                                        |]
          Fim.run program "" `shouldOutput` "Time to throw a party\n"
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
                wrapMethod [text|For every name Name from "Applejack" to "Twilight":
                                I said Name.
                                That's what I did.
                                |]
        Fim.run program2 "" `shouldError`
          "Can only iterate over numbers and characters, instead got string and string"
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
        it "should return arrays" $ do
          let program = wrapClass
                [text|Today I learned something cool.
                      I said something neat.
                      That's all about something cool!

                      I learned something neat with names:
                      Did you know that Twilight's Friends is many names?
                      Twilight's Friends 1 is "Pinkie Pie".
                      Twilight's Friends 2 is "Fluttershy".
                      Twilight's Friends 3 is "Applejack".
                      Twilight's Friends 4 is "Rarity".
                      Twilight's Friends 5 is "Rainbow Dash".
                      Then you get Twilight's Friends!
                      That's all about something neat!
                     |]
          Fim.run program "" `shouldOutput`
            T.concat [ "Pinkie Pie, "
                     , "Fluttershy, "
                     , "Applejack, "
                     , "Rarity, "
                     , "and Rainbow Dash\n"
                     ]
    describe "string concat" $ do
      it "should combine literals" $ do
        let program = wrapMethod [text|I said "Spike is "12" years old"!|]
        Fim.run program "" `shouldOutput` "Spike is 12 years old\n"
      it "should combine variables" $ do
        let program = wrapMethod [text|Did you know that Applejack's apple count is the number 17?
                                       I said "Applejack has "Applejack's apple count" apples"!
                                      |]
        Fim.run program "" `shouldOutput` "Applejack has 17 apples\n"
      it "should support chaining messages" $ do
        let program = wrapMethod [text|I said "Pinkie Pie made "17" cupcakes and "25" muffins"!
                                      |]
        Fim.run program "" `shouldOutput` "Pinkie Pie made 17 cupcakes and 25 muffins\n"
      it "should concat with characters" $ do
        let program = wrapMethod [text|I said 'a'2'z'!
                                      |]
        Fim.run program "" `shouldOutput` "a2z\n"
      it "should support complex expressions" $ do
        let program = wrapMethod [text|I sang "My fact is " 17 is greater than 18'!'.
                                      |]
        Fim.run program "" `shouldOutput` "My fact is false!\n"
    describe "arrays" $ do
      it "should support definitions inline" $ do
        let program = wrapMethod [text|Did you know that Twilight's book collection is the names
                                      "Starswirl's Journal"
                                      and "The Elements of Harmony"
                                      and "Practical Magic"?
                                      I sang Twilight's book collection!
                                      |]
        Fim.run program "" `shouldOutput`
          "Starswirl's Journal, The Elements of Harmony, and Practical Magic\n"
      it "should support definitions by statement" $ do
        let program = wrapMethod [text|Did you know that Applejack's parents had many names?
                                       Applejack's parents 1 is "Pear Butter".
                                       Applejack's parents 2 is "Bright Mac".
                                       I said Applejack's parents!
                                      |]
        Fim.run program "" `shouldOutput` "Pear Butter and Bright Mac\n"
      it "should print singleton arrays as just elements" $ do
        let program = wrapMethod [text|Did you know that Scootaloo's sister has
                                      the names "Rainbow Dash"?
                                      I said Scootaloo's sister.
                                      |]
        Fim.run program "" `shouldOutput` "Rainbow Dash\n"
      it "should error on mismatched types" $ do
        let program = wrapMethod [text|Did you know that Sugercube Corner's Prices is the numbers 17 and "free"?
                                      |]
        Fim.run program "" `shouldError`
          "<Sugercube Corner's Prices> is an array of numbers, but element 2 is a string"
      describe "indexing" $ do
        it "should lookup array by index" $ do
          let program = wrapMethod
                [text|Did you know that The diarchs has the names "Celestia" and "Luna"?
                     I said The diarchs 1!
                     |]
          Fim.run program "" `shouldOutput` "Celestia\n"
        it "should throw an error when looking up an invalid index" $ do
          let program = wrapMethod
                [text|Did you know that The diarchs has the names "Celestia" and "Luna"?
                     I said The diarchs 3!
                     |]
          Fim.run program "" `shouldError` "There is no element 3 of array <The diarchs>"
        it "should throw an error when indexing an invalid type" $ do
          let program = wrapMethod [text|Did you know that Applejack's age is the number 17?
                                        I said Applejack's age 2!
                                        |]
          Fim.run program "" `shouldError` "Cannot index <Applejack's age> of type number"
      describe "assignment" $ do
        it "should update arrays in place" $ do
          let program = wrapMethod
                [text|Did you know that Twilight's Heroes is the names "Celestia" and "Starswirl"?
                     Twilight's Heroes 2 is now "her friends".
                     I sang Twilight's Heroes!
                     |]
          Fim.run program "" `shouldOutput` "Celestia and her friends\n"
        it "should throw an error on updating beyond bounds" $ do
          let program = wrapMethod
                [text|Did you know that The Crusaders is many names?
                     The Crusaders 1 is "Scootaloo".
                     The Crusaders 2 is "Sweetie Belle".
                     The Crusaders 3 is "Apple Bloom".
                     The Crusaders 4 is now "Babs Seed".
                     |]
          Fim.run program "" `shouldError` "There is no element 4 of array <The Crusaders>"
        it "should prevent array assignments of wrong type" $ do
          let program = wrapMethod
                [text|Did you know that Sunburst's favourite constants is the
                     numbers 3.14159 and 2.71828?
                     Sunburst's favourite constants 1 is now "you dork".
                     |]
          Fim.run program "" `shouldError`
            "Can't assign string to variable <Sunburst's favourite constants> of type number"


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
