main = do
  print ((powerOf2 . inc) 4)

inc x = x + 1

powerOf2 x = x * x
