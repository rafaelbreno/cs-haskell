## Hello, World!

### Summary
1. [Introduction](#introduction)
2. [The Code](#the-code)
3. [Real Hello World](#real-hello-world)

### Introduction

As every blog post, documentation, introduction, etc... to a language, we've to write our first _"Hello, World!"_ program. With that, we'll learn in practice:
- How to run Haskell Code
- Where's the starting point of a Haskell program
- What are Type Signatures, and why they are useful for us
- What are Functions (Even though we'll be tackling that in the next chapter.)

### The Code
You can run the code using:
> $ runhaskell hello_world.hs

Also can see the program file itself at [hello_world.hs](./hello_world.hs), and it looks like this:
```haskell 
module Main 
  where

main :: IO ()
main = putStrLn "Hello, World!"
```

#### Code Breakdown
- `module Main`
    - The `module` is a keyword that defines a unit that can export other things, so it can be imported and referenced elsewhere.
    - `Main` is the name of this module.
- `where`
    - Is a keyword to define the scope of something, in this case this means: _The module `Main` is populated by:_
- `main :: IO ()`
    - This is what we call _Type Signatures_, basically manually declaring what type it represents
    - `main` is the name of the item, in this case a _function name_
    - `::` This separates the name of the item, to the type
    - `IO` I/O action
        - 
    - `()` this is an empty tuple, also known as _"unit type" _ or _"unit value"_.
        - we'll see more about `tuples` in the next chapters, but for now, you can think of it being a list that can have various different values.

### Real Hello World
I've read that the _"real" Hello, World_ for FP is the factorial function, so, let's do the same thing.
You can run the code using:
> $ runhaskell fac_world.hs
```haskell
module Main 
  where

fac :: (Integral a) => a -> a
fac n 
  | n < 0 = -1
  | n == 0 = 1
  | otherwise = n * fac (n - 1)
```
- `fac :: (Integral a) => a -> a`
    - `fac` is a function name.
    - `::` separates the function name from its type signature.
    - `(Integral a) =>` is a type constraint, specifying that a must be an instance of the `Integral` typeclass (meaning it supports integral operations).
    - `a -> a` indicates that fac takes an argument of type `a` and returns a value of type `a`.
- `fac n`
    - Defines the function `fac` with the argument `n`.
    - The subsequent lines use guards to define different behavior based on the value of `n`.
- `| n < 0 = -1`
    - If `n` is less than `0`, `fac n` returns -1.
- `| n == 0 = 1`
    - If `n` is `0`, `fac n` returns `1`.
- `| otherwise = n * fac (n - 1)`
    - For all other values of `n`, `fac n` returns `n` multiplied by `fac (n - 1)`, recursively computing the factorial.

<!--- Here, also add the line by line explanation. --->

## References
- [Haskell Cheatsheet](https://hackage.haskell.org/package/CheatSheet-1.8/src/CheatSheet.pdf)
