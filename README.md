# happy-space expression parser

this is an infix expression parser. it supports

* the operations `+`, `-`, `*`, `/`, `%`, `^`
* conventional operator precendence
* parentheses
* whitespace precedence

## dependencies

* ghc
* make
* dash (for tests)

## to build

run `make` in the top level of the repo. this will create `bin/hs_expr`.

you can also `make test`, if you have the appropriate dependencies.


## to run

there are three ways to invoke `hs_expr`:

* running `hs_expr` will produce an interactive repl;
* `hs_expr "1+2"` will parse all the expressions passed as arguments;
* `hs_expr -` reads once from standard in and parses that.
