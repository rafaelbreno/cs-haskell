## About

### Summary
1. [About Functional Programming](#about-functional-programming)
2. [About Haskell](#about-haskell)
3. [Haskell Features](#haskell-features)
9. [References](#references)

### About Functional Programming
_"In computer science, functional programming is a programming paradigm where programs are constructed by applying and composing functions. [...]"_
_"[...] In functional programming, functions are treated as first-class citizens, meaning that they can be bound to names (including local identifiers), passed as arguments, and returned from other functions, just as any other data type can. [...]"_

### About Haskell
_"Haskell is a general-purpose, statically typed, purely functional programming language with type inference and lazy evaluation. [...]"_
__Pure function__ _"[...] consists of ensuring that functions, inside the functional paradigm, will only depend on their arguments, regardless of any global or local state. [...]"_, meaning that function won't have side-effects. _"[...] Purely functional data structures are persistent. Persistency is required for functional programming; without it, the same computation could return different results. [...]"_

### Haskell Features
- __Statically typed.__ _"Every expression in Haskell has a type which is determined at compile time. All the types composed together by function application have to match up. If they don't, the program will be rejected by the compiler. Types become not only a form of guarantee, but a language for expressing the construction of programs."_
- __Purely functional.__ _"Every function in Haskell is a function in the mathematical sense (i.e., "pure"). Even side-effecting IO operations are but a description of what to do, produced by pure code. There are no statements or instructions, only expressions which cannot mutate variables (local or global) nor access state like time or random numbers."_
- __Type inference.__ _"You don't have to explicitly write out every type in a Haskell program. Types will be inferred by unifying every type bidirectionally. However, you can write out types if you choose, or ask the compiler to write them for you for handy documentation."_
- __Concurrent.__ _"Haskell lends itself well to concurrent programming due to its explicit handling of effects. Its flagship compiler, GHC, comes with a high-performance parallel garbage collector and light-weight concurrency library containing a number of useful concurrency primitives and abstractions."_
- __Lazy.__ _"Functions don't evaluate their arguments. This means that programs can compose together very well, with the ability to write control constructs (such as if/else) just by writing normal functions. The purity of Haskell code makes it easy to fuse chains of functions together, allowing for performance benefits."_
- __Packages.__ _"Open source contribution to Haskell is very active with a wide range of packages available on the public package servers."_

### References
- [Functional Programming - Wikipedia](https://en.wikipedia.org/wiki/Functional_programming)
- [Functional Programming - Serokell](https://serokell.io/blog/introduction-to-functional-programming)
- [Haskell - Wikipedia](https://en.wikipedia.org/wiki/Haskell_(programming_language))
- [Haskell](https://www.haskell.org/#:~:text=%C2%BB-,Features,-Statically%20typed)
