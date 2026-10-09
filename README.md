# MiniLisp

A tree-walking interpreter for a subset of Lisp, written from scratch in Haskell.

## Features

* **Parser:** Tokenizes and parses parenthesized expressions and literals (integers, booleans, strings, and quoted lists).
* **Lexical Scoping & Closures:** Supports anonymous functions (`lambda`) with lexical environment capture.
* **First-Class Lists:** Built-in list primitives including `cons`, `car`, `cdr`, `null?`, and `list`, with `'` quoting syntax.
* **Conditionals & Variables:** Supports variable definitions (`define`) and branching (`if`).
* **Interactive REPL:** Features runtime error handling and an AST inspector (`:ast`).

## Getting Started

### Prerequisites

GHC (`ghc` and `ghci`) installed.

### Running with GHCi

1. Load the project:
   ghci Main.hs

2. Start the REPL:
   main

### Compiling to Binary

ghc Main.hs -o minilisp
./minilisp

## Example REPL Session

mini> (+ 5 (* 3 4))
17

mini> (define nums '(10 20 30))
(10 20 30)

mini> (car nums)
10

mini> (cdr nums)
(20 30)

mini> (define makeAdder (lambda (n) (lambda (x) (+ n x))))
<closure (n)>

mini> (define add10 (makeAdder 10))
<closure (x)>

mini> (add10 25)
35

mini> :ast (+ 1 2)
List [Var "+",LitInt 1,LitInt 2]

mini> :quit
Exiting.
