add :: Int -> Int -> Int -> Int
add x y z = x + y -z 

main :: IO ()
main = do
  let result = add <$> Just 2 <*> Just 3 <*> Just 4
  print result -- Just 5

foo = do
    putStrLn("Test in foo")
    return 3

foo2 = do 
    putStrLn("Test in foo")
    return 3

bar =
   do
      putStrLn("Test in bar")
      v <- foo
      putStrLn ("v is "++show v)
      return ("v^3 is "++show (v^3))

bar2 = do
        putStrLn("Test in bar")
        v <- foo
        -- w <- 7
        putStrLn ("v is "++show v)
        return ("v^3 is "++show (v^3))

afun :: IO Integer
afun =
  do
    aaa <- return 5
    return (aaa+4)
