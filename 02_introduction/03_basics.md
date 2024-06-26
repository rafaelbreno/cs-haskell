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
5. [If Expressions](#if-expressions)
6. [Scope](#Scope)
7. [Where vs Let](#where-vs-let)
  1. [Example Where vs Let](#example-where-vs-let)
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

### Comments

#### Single Line Comment
```haskell
age = 55   -- age is equal 55
pi = 3.14  -- pi is equal to 3.14
-- idk = 7 -- idk is not defined.
```

#### Multi/Inline Comment
```haskell
{-- 
age = 55   -- age is equal 55
pi = 3.14  -- pi is equal to 3.14
-- idk = 7 -- idk is not defined.
--}
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
Internally, Lists (`[a]`) are _linked lists_, making index-based lookups really slow, and _prepending_ elements really fast.
<!--  
TODO: Add a link to somewhere showing that `List` is in fact _linked list_.
TODO: Add a link to a chapter using Data.Vector
-->

#### Operators

##### Basic
|Name         |Operators                                                 |
|-------------|----------------------------------------------------------|
|Arithmetic   |`+`, `-`, `*`, `/`                                        |
|Boolean      |`&&`, `||`, `==`, `/=`(different), `not`(boolean negation)|
|List Concat  |`++`                                                      |
|String Concat|`++`, remember, a `String` is a List of Char: `[Char]`    |

Note: These operators are called _infix operator_, _"[...]Haskell allows two-argument functions to be written as infix operators placed between their arguments [...]"_
```haskell 
2 == 2   -- this
(==) 2 2 -- is equal to this
```

#### Not Basic

|Name                  | Operator | Signature                       | Example                                 |
|----------------------|----------|---------------------------------|-----------------------------------------|
| Function operator    | `$`      | `(a -> b) -> a -> b`            | `f ( g ( h ( x ) ) )` = `f $ g $ h $ x` |
| Function composition | `.`      | `(b -> c) -> (a -> b) -> a -> ` | `(chr . (+1) . ord) 'A'`                |

[Examples by HaskellCats](https://github.com/haskellcats/haskell-operators/blob/master/examples.md)

#### If Else
_"`if-then-else` in Haskell works very similar to other languages. For example, here’s a function to return `"odd" / "even"` depending upon the input number:"_

```haskell
module Throwaway where

checkNumber :: Int -> String -- you can ignore this for now
checkNumber y =
  if (mod y 2) == 0 then 
    "even"
  else 
    "odd"
```

#### Scope
Scope can be defined as an area where something can be accessible

##### Where
```haskell
-- `value` isn't accessible here
sum2 x y = (x + y + value + otherValue) -- we can refer to `value` here, and access `otherValue`
  where 
    two = 2 -- defining `value`

-- Defining `otherValue`
otherValue = 10

-- This won't work:
addValue x = x + value -- won't work because value isn't in this scope
```

#### Where vs Let
- `let ... in ...` is an expression, that is, it can be written wherever expressions are allowed. 
- `where` is bound to a surrounding syntactic construct, like the _pattern matching_ line of a function definition.

Example:
```haskell
fibWhere = (map fib' [0 ..] !!)
  where
    fib' 0 = 0
    fib' 1 = 1
    fib' n = fib (n - 1) + fib (n - 2)

fibLet = 
  let fib' 0 = 0
      fib' 1 = 1
      fib' n = fib (n - 1) + fib (n - 2)
  in (map fib' [0 ..] !!)
```

You can run the `where vs let` example with:
```shell
$ ghc -o where_let where_let.hs
$ ./where_let
```

#### Guards
Haskell syntax allows us to use _boolean operators_ to write simpler functions, for example 

```haskell
fib :: Int -> Int
fib n 
    | n < 0     = error "negative number not allowed"
    | n < 2     = n
    | otherwise = fib ( n - 1 ) + fib ( n - 2 )
```

The `otherwise` will only be reached if none of the other options are `True`

##### Guards with Where
```haskell
fib :: Int -> Int
fib n 
    | check n   = error "negative number not allowed"
    | check n   = n
    | otherwise = fib ( n - 1 ) + fib ( n - 2 )
    where check n = ( n < 0 ) || ( n < 2 ) -- this is overkill, but its an example
```

### Type Annotations
_"Type annotations in Haskell are written as `term :: type`"_
So for example, let's add a type annotation for one of the functions that we've written before.
```haskell
-- A non typed Factorial function
-- fac 0 = 1
-- fac n = n * fac (n-1)

-- fac is our term
-- Int -> Int is fac's type, meaning:
-- -- Receive 1 parameter of type Int
-- -- Return a value of type Int
fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n - 1)

-- sumTwoValues is our Term
-- Int -> Int -> Int is sumTwoValues's type, meaning:
-- -- Receive 2 parameters of type Int
-- -- Return a value of type Int
sumTwoValues :: Int -> Int -> Int
sumTwoValues x y = x + y
```

Run the example with:
```shell
$ ghc -o type_annotation type_annotation.hs
$ ./type_annotation
```

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
- [Where vs Let](https://wiki.haskell.org/Let_vs._Where)
- [Haskell Type Annotations](https://www.haskell.org/hugs/pages/users_guide/type-annotations.html)
- [Haskell Type Annotations - Gist](https://gist.github.com/CMCDragonkai/5049f290ce9b51ccd7a9)
