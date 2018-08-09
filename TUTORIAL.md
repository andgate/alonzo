# Tutorial

## Install

First, you need to install ghc and cabal, preferably the most recent.
I recommend using ubuntu to access them.

## Enter the lambda

To use the langauge, for now, you have to go into the repl.

With the necessary tools installed, the repl can be started via the command:

```
cabal new-run alci
```

This will load the prelude, and drop you into the repl.

## Basic Syntax

The basic syntax of Alonzo is describe by the following syntax tree...

```
Def d ::= name = term

Term t ::=  var
            var var
            \var. var
            let var = t, var = t, ... in t
```

In Alonzo, the language is restricted to only having variables, function calls, and anonymous functions. This is called the untyped lambda calculus.

## Hello world

With the lambda calculus, we can do anything. How about hello world!

hello = #print "hello world"

Very simple!

