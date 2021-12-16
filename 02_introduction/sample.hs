module Main 
  where

main = print (fac 4)

fac n = 
  if n == 0 then
    1
  else
    n * fac(n-1)
