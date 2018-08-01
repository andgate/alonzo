## Alonzo Tutorial

Alonzo is a purely untyped, functional programming language. This language strives to model the lambda calculus, developed by Alonzo Church. By equipping the lambda calculus with a simple syntax for objects, Alonzo provides a solid basis for programming in a dynamic, functional style.

# Enter The Lambda

As a purely functional language, Alonzo programs are written using functions, which are described by expressions, also called terms. Here is one example of a term...
```
repl> 1 + 5
6
```
Terms are very powerful tools for expressing computations. Our terms are written in the language of the lambda calculus. We have variables like `x`, `y`, `foo`, etc. Any lower case string is essentially a variable in Alonzo.
Then we have data constructors. `Just x`, `Nothing`, `List cons rest` are all examples. Data constructors are the meat of most programs.
