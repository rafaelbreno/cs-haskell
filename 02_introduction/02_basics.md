## Basics

### Summary
1. [Haskell Interpreter](#haskell-interpreter)
  1. [What is GHCi](#what-is-ghci)
  2. [Using GHCi](#using-ghci)
  3. [Loading files into GHCi](#loading-files-into-ghci)
2. [Compiling Haskell Programs](#compiling-haskell-programs)
  1. [Using GHC](#using-ghc)
  2. [Stack](#stack)
3. [Haskell Main](#haskell-main)
4. [Expressions](#expressions)
  1. [Primitive Types](#primitive-types)
  2. [Operators](#operators)
9. [References](#references)

### Haskell Interpreter

#### What is GHCi
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

#### Using GHCi
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

#### Loading Files into GHCi
Let's create the following [file](./sample.hs)
```haskell
-- sample.hs
fac n = 
  if n == 0 then
    1
  else
    n * fac(n-1)
```

If in a project we want to test one function from a file, you may want to run it using the `ghci`, so after creating the `sample.hs` file, run the `ghci`, and let's try to call the `fac` function
```shell
ghci> fac 4

<interactive>:1:1: error: Variable not in scope: fac :: t0 -> t
```

Well, it didn't work, why? Because we didn't imported it, so let's import the file using`ghci> :load sample.hs`
```shell
ghci> :load sample.hs 
[1 of 1] Compiling Main             ( sample.hs, interpreted )
Ok, one module loaded.
ghci> fac 4
24
```

### Compiling Haskell Programs

#### Using GHC
Let's first create a simple Haskell program
```haskell
-- sample02.hs
module Main
  where

main = print (fac 4)

-- Just using some pattern matching
fac 0 = 1
fac n = n * fac (n-1)
```

Now with this little program we can compile and run it with:
```shell
$ ghc -o sample02 sample02.hs
$ ./sample02
24
```

The compiling process will generate other 2 files `*.h1` and `*.o` that can be deleted after the compiling.

#### Stack
_"[Stack](https://docs.haskellstack.org/en/stable/README/) is a cross-platform program for developing Haskell projects. It is aimed at Haskellers both new and experienced."_
So to start a simple example, create a folder(in this case you can find a folder called `sample-stack/`) with:
> $ stack new sample-stack
This will generate a default stack project structure, then to run it you can do this:
```shell
$ stack new sample-stack
$ cd sample-stack/
$ stack run
someFunc
```

Or you can build it into a binary file with:
```shell
$ stack build
$ stack exec sample-stack-exe
```

### Haskell Main
_"A Haskell program needs to have an \"entry point\" called `main`."_
```haskell
module Main where

main = putStrLn "Start application"
```

### Expressions
[_"programs in functional languages are primarily built out of expressions"_](https://cs3110.github.io/textbook/chapters/basics/expressions.html#:~:text=programs%20in%20functional%20languages%20are%20primarily%20built%20out%20of%20expressions), Haskell has a definition of [expressions in the language](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html). 

#### Primitive Types
_In Haskell all data-types start with capital letter!_
|Type   |Example                                    |
|-------|-------------------------------------------|
|Int    |`1`,`2`,`-3`                               |
|Float  |`1.0`, `3.14`, `-27.64`                    |
|Bool   |`True` or `False`                          |
|Char   |`'a'`, `'b'`, `'\t'`                       |
|String |List of `Char`(`[Char]`), `"foo"`, `"bar"` |

In Haskell we've [Lists, not Arrays](https://www.vacationlabs.com/haskell/basic-types-and-functions.html#lists-not-arrays), `Lists` only accepts values of the same type.

#### Operators

##### Basic
|Name         |Operators                                                 |
|-------------|----------------------------------------------------------|
|Arithmetic   |`+`, `-`, `*`, `/`                                        |
|Boolean      |`&&`, `||`, `==`, `/=`(different), `not`(boolean negation)|
|List Concat  |`++`                                                      |
|String Concat|`++`, remember, a `String` is a List of Char: `[Char]`    |

#### Not Basic
- `TODO:`
  - Add some operators here

### References
- [Typeclass Constraint - Haskell](https://en.wikibooks.org/wiki/Haskell/Classes_and_types)
- [Typeclass Constraint - Stackoverflow](https://stackoverflow.com/questions/9142731/what-does-the-symbol-mean-in-haskell)
- [Eq](https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Eq.html)
- [Num](https://hackage.haskell.org/package/base-4.16.0.0/docs/GHC-Num.html)
- [Stack](https://docs.haskellstack.org/en/stable/README/)
- [Haskell Main](https://typeclasses.com/beginner-crash-course/main)
- [Expressions](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html)
- [Primitive Data Types](https://www.vacationlabs.com/haskell/basic-types-and-functions.html)
- [Data.Primitive.Types](https://hackage.haskell.org/package/primitive-0.7.3.0/docs/Data-Primitive-Types.html)
- [Operator](https://imada.sdu.dk/~rolf/Edu/DM22/F06/haskell-operatorer.pdf)
- [Operators - FP Complete](https://www.fpcomplete.com/haskell/tutorial/operators/)
