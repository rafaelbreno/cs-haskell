main = do
  putStrLn (bar (2::Int))
  putStrLn (bar "aaaa")

class Foo a where
  bar :: a -> String

instance Foo Int where
  bar x = show (x + 1)

instance Foo String where
  bar x = show (x ++ " hello!")
