## Basics

### Summary
1. [Haskell Interpreter](#haskell-interpreter)
9. [References](#references)

### Haskell Interpreter
Haskell has `ghci`(GHC Interpreter executable), so if you run `ghci` in your terminal you will be presented with the following prompt:
```shell
Loaded package environment from /path/to/.ghc/x86_64-linux-9.0.1/environments/default
GHCi, version 9.0.1: https://www.haskell.org/ghc/  :? for help
ghci> 
```
With this, we can start playing with Haskell without writing a `.hs` file and running it, before we start there's a config that I think will be helpful, if you type `:?` in your terminal, will be printed a list of config, one of those items is the `:set +t`: _"print type after evaluation"_. This config will show the type of any declaration that was made.
So, in you terminal run:
```shell
ghci> :set +t
```

To start, let's define a factorial function
```shell
ghci> let fac n = if n == 0 then 1 else n * fac(n-1)
fac :: (Eq p, Num p) => p -> p
```
Let's try to understand the output: `fac :: (Eq p, Num p) => p -> p`
- `fac` - The binding name
- `::` - keyword that defines the type of that binding
- `(Eq p, Num p) =>`
  - This a typeclass constraint
  - `(Eq p, Num p)` works with any `p`, that implements equality and inequality(`Eq`) and numerical(`Num`)
  - `=>` - apply those types to the right
- `p -> p`
  - Receives a `p` and returns a `p`
  - So if `p` is an `Integer` it will return `Integer`

We can see another types too, for example:
```shell
ghci> let num = 10
num :: Num p => p
ghci> let str = "Hello, World!"
str :: String
ghci> let float = 3.14
float :: Fractional p => p
```

### References
- [Typeclass Constraint - Haskell](https://en.wikibooks.org/wiki/Haskell/Classes_and_types)
- [Typeclass Constraint - Stackoverflow](https://stackoverflow.com/questions/9142731/what-does-the-symbol-mean-in-haskell)
- [Eq](https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Eq.html)
- [Num](https://hackage.haskell.org/package/base-4.16.0.0/docs/GHC-Num.html)
