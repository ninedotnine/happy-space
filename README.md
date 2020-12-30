# happy-space

this is a command-line calculator. it supports

* the infix operations `+`, `-`, `*`, `/`, `%`, `^`
* the prefix operators `~` (negate) and `!` (factorial)
* conventional operator precendence
* parentheses
* whitespace precedence

## whitespace precedence?

that's right!

```
> 3+6/3
5
> 3 + 6/3
5
> 3+6 / 3
3
```

but:

```
> 1+ 2
"input" (line 1, column 4):
unexpected whitespace after `+`
> 1 +2
"input" (line 1, column 4):
unexpected "2"
expecting space after `+`
```

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
* `hscalc "1+2"` will evaluate all the expressions passed as arguments;
* `hscalc -` will read once from standard input and evaluate that.
