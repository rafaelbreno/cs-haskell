## Function
You may have seen some examples with functions in `Introduction` chapter, but now we'll get into depth in this Chapter

### Summary
1. [Definition](#definition)
  1. [Untyped definition](#untyped-definition)
  2. [Typed definition](#typed-definition)
2. [Anonymous Functions](#anonymous-functions)
9. [References](#references)

### Definition

```haskell
-- <function_name> <params> = <function_body>
fac 0 = 1
fac x = x * fac (x - 1)
-- fac x = if x == 0 then 1 else x * fac (x - 1)
```

#### Untyped definition
In Haskell you can declare some functions without defining a type, so Haskel will infer the types of:
- Parameters
- Returned value
```haskell
facUntyped 0 = 1
facUntyped 1 = 1
facUntyped x = x * facUntyped (x - 1)
```

Semantics:
- `<function_name> p1 p2 ... pn = e`
  - `<function_name>` being the name bound to that functions
  - `p1 p2 ... pn` received values
  - `= e` function body

#### Typed definition
If we wanted to define the types for the parameters and returned value we could do something like this:
```haskell
facType :: Int -> Int
facType 0 = 1
facType 1 = 1
facType x = x * facUntyped (x - 1)
```

Semantics:
- `<function_name> :: P1 -> P2 -> ... -> PN -> R`
  - `<function_name>` being the name bound to that functions
  - `::` _infers something_
  - `P1 ... PN` received types
  - `R` returned type

### Anonymous Functions
_"An anonymous function is a function without a name. t is a Lambda abstraction and might look like this: _ `\x -> x + 1`_. (That backslash is Haskell's way of expressing a λ and is supposed to look like a Lambda.)"_

```haskell
main = do
  print ((\x -> x + 1) 4)
  print ((\x y -> x + y) 4 5)
  print (namedAnonymous 5)

namedAnonymous = \x -> x * x
```

Syntax:
- `\x y ... z -> e`

### References
- [Real World Haskell - Functions](http://book.realworldhaskell.org/read/types-and-functions.html)
- [Anonymous Functions](https://wiki.haskell.org/Anonymous_function)
