fib :: Int -> Int
fib n 
    | check n   = error "negative number not allowed"
    | check n   = n
    | otherwise = fib ( n - 1 ) + fib ( n - 2 )
    where check n = ( n < 0 ) || ( n < 2 )
