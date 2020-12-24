# happy-space

this is an calculator. it supports

* the infix operations `+`, `-`, `*`, `/`, `%`, `^`
* the prefix operators `~` (negate) and `!` (factorial)
* conventional operator precendence
* parentheses
* whitespace precedence

## dependencies

* ghc
* make
* dash (for tests)

## to build

run `make` in the top level of the repo. this will create `bin/hs_calc`.

if you experience problems with linking, you can try `make HSFLAGS=-static`.

you can also `make test`, if you have the appropriate dependencies.

## to run

there are three ways to invoke `hscalc`:

* running `hscalc` will produce an interactive repl;
* `hscalc "1+2"` will evaluated all the expressions passed as arguments;
* `hscalc -` reads once from standard in and parses that.
