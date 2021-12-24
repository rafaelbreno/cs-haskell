## Function
You may have seen some examples with functions in `Introduction` chapter, but now we'll get into depth in this Chapter

### Summary
1. [Definition](#definition)
  1. [Untyped definition](#untyped-definition)
  2. [Typed definition](#typed-definition)
2. [Anonymous Functions](#anonymous-functions)
3. [Pipeline](#pipeline)
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

### Pipeline
_"This can be done with any two functions, where the argument type of the first is the return type of the second. The newly created function takes what the second function would as a parameter and feeds it through the second function, then the result of the second function through the first function, and returns the result of the first function."_
So, having +2 functions, we can do something like this:
- `(f1 . f2 . ... . fn) v`
- This would run like:
  - `v1 = fn v`
  - `v2 = f(n-1) v1`
  - `...`
  - `vn = f1 v(n-1)`

```haskell
main = do
  print ((powerOf2 . inc) 4)

inc x = x + 1

powerOf2 x = x * x
```

### References
- [Real World Haskell - Functions](http://book.realworldhaskell.org/read/types-and-functions.html)
- [Anonymous Functions](https://wiki.haskell.org/Anonymous_function)
- [Function Composition](https://wiki.haskell.org/Function_composition)
