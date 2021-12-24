main = do
  print ((\x -> x + 1) 4)
  print ((\x y -> x + y) 4 5)
  print (namedAnonymous 5)

namedAnonymous = \x -> x * x
