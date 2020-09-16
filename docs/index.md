# friendshipismonadic
## Introduction

I've just finished version 0.1 of my passion project for the last month and a
half: A Haskell interpreter for the esolang [FiM++][esolang]. I call it, of
course, [friendshipismondaic][github]

FiM++ is a language based on My Little Pony: Friendship is
Magic. It's a prose language that takes the form of the friendship reports
Twilight Sparkle wrote to her mentor Princess Celestia in early seasons of the
show. Here's an example Hello World program.

```
Dear Princess Celestia: Hello World!

Today I learned something simple.
I said “Hello, Equestria!”!
That's all about something simple!

Your faithful student, Twilight Sparkle.
```

There is a relatively [comprehensive spec][spec] for the language, which comes
as a bit of a surprise. However, diving in deeper, a number of, shall we say,
idiosyncrasies become obvious.

One is the obvious Java heritage of the original writers. Why would a toy
interpreted language need multiple inheritance, or interfaces? Why is there
a switch statement but no array mutation?

The spec also under-specifies the syntax in some places. `<value>` is used a lot
throughout, but its definitions tend to be contradictory: sometimes it's just
literals and variables, sometimes any expression is valid. I did my best to make sense
of the spec, as well as [document my divergences][errata].

[esolang]: https://esolangs.org/wiki/FiM%2B%2B
[github]: https://github.com/stillinbeta/friendshipismondaic
[spec]: https://docs.google.com/document/d/1gU-ZROmZu0Xitw_pfC1ktCDvJH5rM85TxxQf5pg_xmg
[errata]: https://github.com/stillinbeta/friendshipismonadic/blob/master/ERRATA.md
[parsec]: https://hackage.haskell.org/package/parsec

## Show me the code!

* [It's on Github!][github]
* Hackage coming soon, maybe!
* Check out the [example programs][examples]
* (Linux) [get a binary][releases]

[github]: https://github.com/stillinbeta/
[examples]: https://github.com/stillinbeta/friendshipismonadic/tree/master/examples
[releases]: https://github.com/stillinbeta/friendshipismonadic/releases
## Architecture

### Lexing
My initial pass at a parser combined the lexing and parser into a single step.
The library [Parsec][parsec] encourages this by operating mostly over
characters. However, this approach eventually proved too cumbersome and I split
out the lexer into a [`context-free parser`][lexer].

This language is actually incredibly difficult to lex. Variables can contain
whitespace, reserved words are actually reserved phrases, and suffixes like
"n't" and "es" at the end of words have syntactic meaning. One of the things that
defines boundaries is reserved words: A variable cannot contain "is", for
example, because then it would be impossible to parse "Did you know that
variable is the number 1"? (the variable declaration syntax).

To solve this, I made a list of [reserved words][reserved]. I use template
Haskell to turn these into a datatype like `R_is` and `R_Did_you_know_that` that
can easily be matched in the lexer without duplication.

The language has a number of "synonyms", that is, words or phrases that are
syntactically equivalent. For example, "is" above could also be "was", "has",
"had", "like", "likes", or "liked". To the extent that's possible, these synonyms
are matched and collapsed in the lexer. However, some words like "and" have multiple syntactic
definitions, even though they're synonyms, so this must be handled in the parser.

[lexer]: https://github.com/stillinbeta/friendshipismonadic/blob/master/src/Language/Fim/Lexer.hs#L81-L252
[reserved]: https://github.com/stillinbeta/friendshipismonadic/blob/master/src/Language/Fim/Lexer/Reserved/List.hs

### Parsing

The lexer emits a stream of [tokens][tokens] (along with some bookkeeping needed
for error messaging). Then Parsec is once again used to construct an
[AST][types]. The parser modules are separated by hierarchy: [Classes][classes],
[Methods][methods], [Statements][statements], [Values][value],
[Literals][literal]. The bulk of the difficult processing takes place in
Statements and Values, because these are the primary recursive data structures.

Parsec allows the construction of parsers that are relatively DSL-like using do
notation. However, there's a lot of window-dressing needed to manage the type
system and construct the ASTs properly.

[tokens]: https://github.com/stillinbeta/friendshipismonadic/blob/master/src/Language/Fim/Lexer/Token.hs
[types]: https://github.com/stillinbeta/friendshipismonadic/blob/master/src/Language/Fim/Types.hs
[classes]: https://github.com/stillinbeta/friendshipismonadic/blob/master/src/Language/Fim/Parser/Class.hs
[methods]: https://github.com/stillinbeta/friendshipismonadic/blob/master/src/Language/Fim/Parser/Methods.hs
[statements]: https://github.com/stillinbeta/friendshipismonadic/blob/master/src/Language/Fim/Parser/Statement.hs
[value]: https://github.com/stillinbeta/friendshipismonadic/blob/master/src/Language/Fim/Parser/Value.hs
[literal]: https://github.com/stillinbeta/friendshipismonadic/blob/master/src/Language/Fim/Parser/Literal.hs


### Evaluating
Together, the parsing and lexing steps represent most of the difficulty in
interpreting FiM++. Once the AST is constructed, FiM++ is basically the same as
any other toy language. However, I don't have any formal background in compiler
or interpreter design, so I ended up kind of winging it. The spec does not say
anything about the evaluation model, just the syntax.

The Evaluator can be run one of two ways, depending on the type of the evaluation monad.
[`RunClassIO`][io] directly reads input and output from stdin and stdout, and
this is what is used by the `fim` binary. [`RunClass`][rws] uses a `Reader` for input
and a `Writer` for output, and is used primarily for testing.

The evaluator is structured hierarchically similar to the Parser, but with `Value`
and `Statement` combined into `Language.Fim.Eval.Statement`. This is because
Statement and Value are mutually recursive, so having them in separate modules
would result in an import loop.

Because the language is not necessarily static, and to divorce the language from
Haskell's type system, all values are [boxed][box]. All methods either
explicitly return a boxed value, or implicitly return the null value. This is recorded
by having all statements be of type Maybe Value. Nothing means there's no return, Just Value
means execution should halt and pass up to the next level.

Method calls are relatively simplistic: The variables are stored, new variables
laid in based on the methods arguments, and then swapped back after execution.
There is no stack, or stack traces. Other than methods, the variable space is
completely flat: Variables introduced in conditionals and loops will persist
after their execution.

[io]: https://github.com/stillinbeta/friendshipismonadic/blob/master/src/Language/Fim/Eval.hs#L40-L45
[rws]: https://github.com/stillinbeta/friendshipismonadic/blob/master/src/Language/Fim/Eval.hs#L52-L54
[box]: https://github.com/stillinbeta/friendshipismonadic/blob/master/src/Language/Fim/Eval/Types.hs#L34-L41

### Testing
#### QuickCheck (Hedgehog)
The parser and lexer are tested using [Hedgehog][hedgehog], a QuickCheck-esque
framework that automatically shrinks test cases to the smallest example size.

My initial approach was to use the typical parse (prettyprint ast) = ast
approach. Because there are so many synonyms and duplicate syntaxes, my options
were either a non-pure pretty printer, or to encapsulate all possible
expressions within the AST itself. The results were... [messy][oldast].

I stumbled upon the current approach of having [the generator for the AST also
generate a program representation](generator). All the generators produce a `WithText`
value, which includes both the value and text that represents it. At pretty much
every step the transformations up the AST are non-trivial, so WithText doesn't
implement `Functor` or any other classes that would make combinations easier.

The Hedgehog generators are approximately the dual of the parser, but they do
not share any code. This is intentional: it means the two are checks on each
other. I tried to write both directly from the spec and not based on the code
I'd generated before. I also used a TDD approach, where I would write the
generator first, then the lexer, then the parser, and then write the functional
tests after QuickCheck passed.

[hedgehog]: http://hackage.haskell.org/package/hedgehog-0.5.1
[oldast]: https://github.com/stillinbeta/friendshipismonadic/blob/c63fd2018909b766906a0bdc8ffc0a8400683b99/src/Language/Fim/Types.hs#L29-L34
[generator]: https://github.com/stillinbeta/friendshipismonadic/blob/master/test/Language/Fim/Parser/Gen.hs

### HSpec

The [hspec tests][hspec] are verbose but relatively straight forward. The only
helper functions assist with the Java-esque method and class boiler plate. They
provide not just integration tests of the parser and lexer, but also are the
only tests of the interpreter. I could not find any information about pragmatically
generating tests for interpreters in the QuickCheck style, and I suspect it is not possible
without effectively writing a second interpreter.

The Hspec tests, by their nature, demo every syntactic element implemented by `friendshipismonadic`,
and so is a good place to start getting familiar with the language!

[hspec]: https://github.com/stillinbeta/friendshipismonadic/blob/master/test/Spec.hs
