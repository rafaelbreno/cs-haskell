module Lib
    ( someFunc
    , a
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

let a y = (mod y 2) == 0
