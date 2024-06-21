module Main 
  where

fac :: (Integral a) => a -> a
fac n 
  | n < 0 = -1
  | n == 0 = 1
  | otherwise = n * fac (n - 1)
  

main :: IO ()
main = putStrLn "Hello, World!"
