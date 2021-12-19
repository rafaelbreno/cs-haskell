main = do
  print (fibWhere 7)
  print (fibLet 7)

fibWhere = (map fib' [0 ..] !!)
  where
    fib' 0 = 0
    fib' 1 = 1
    fib' n = fib' (n - 1) + fib' (n - 2)

fibLet = 
  let fib' 0 = 0
      fib' 1 = 1
      fib' n = fib' (n - 1) + fib' (n - 2)
  in (map fib' [0 ..] !!)
