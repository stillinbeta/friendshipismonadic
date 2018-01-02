# friendshipismonadic

A [FiM++][fimpp] interpreter written in Haskell, using Parsec as a parser.

[fimpp]: https://esolangs.org/wiki/FiM%2B%2B

## Dependencies

This project uses [stack][stack], a tool for repeatable, cross-platform builds of haskell projects. You'll need to [install][install] `stack` to build the binaries or run the tests.

[stack]: https://docs.haskellstack.org/en/stable/README/
[install]: https://docs.haskellstack.org/en/stable/README/#how-to-install

## Installation

```shell
stack install

fim examples/helloworld.fim
```

## Run tests
```shell
stack test
```

## Status

### What works

* Boolean, Character, Number, and String literals
* Assigning to/reading from variables
* Arithmatic Operators
* Comparison Operators
* Boolean Operators
* Branching Statements
* While and Do While, and For loops
* User Input

### What doesn't work

* Arrays
* Switches
* Calling Methods
* Increment/Decrement
* Comments

### What I'm not gonna bother with
* Interfaces (what does this even mean in a language without objects?)
* Inheritence (Not much point without libraries)
* More than one class (no syntax for calling other classes or creating objects)
