module Main 
  where

main = print (fac 4)

fac 0 = 1
fac n = n * fac (n-1)
