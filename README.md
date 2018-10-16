# hse-parsing
A simple recursive descent parser. Written for the formal languages course in HSE. The language is described with the following grammar:

```
S -> ExprGroup | \epsilon

ExprGroup -> BaseExpr (; ExprGroup)?

BaseExpr -> Ident = BaseExpr
          | Ident ++ ListExpr  // :(
          | NumExpr 
          | ListExpr

ListExpr -> Ident = ListExpr
          | List (++ ListExpr)?

List -> '[' ListCore ']'
      | Ident

ListCore -> BaseExpr (, ListCore)?
          | \epsilon

NumExpr -> Ident = NumExpr
         | Term ((+ | -) NumExpr)?

Term -> Exp ((* | /) Term)?

Exp -> Factor (^ Exp)?

Factor -> -Factor 
        | Ident 
        | Num 
        | '(' NumExpr ')'

Ident -> ('a' | 'b' | ... | 'z') ('a' | ... | 'z' | '0' | ... | '9')*

Num -> ('0' | '1' | ... | '9')+
```

Running the build script `build.sh` generates an executable `Main`. `Main` parses several inputs specified and terminates.

To run parser on your input, load `Main` into the interpreter `ghci` and execute `parse <input>` or modify `Main.hs`.

This should work on any version of the haskell compiler, but has only been tested on `ghc 8.4.2`.
