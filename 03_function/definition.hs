main = do
  print (facUntyped 4)

facUntyped 0 = 1
facUntyped 1 = 1
facUntyped x = x * facUntyped (x - 1)

facType :: Int -> Int
facType 0 = 1
facType 1 = 1
facType x = x * facUntyped (x - 1)
