-- | 

module Type where
import Test.QuickCheck
import Data.Bool (Bool)
import GHC.Base (Float)

answ :: Integer
answ = 42

square :: Double -> Double
square x = x*x

fourth x = square (square x)

foo :: Double -> Double -> Double
foo x y = x + 1000 *y

myNot True = False
myNot False = True
prop_myNot :: Bool -> Bool
prop_myNot x = not x == myNot x

myif True then_exp else_exp = then_exp
myif False then_exp else_exp = else_exp

gfac n = myif (n<=0) 1 (n*gfac (n-1))

-- Excercise
-------- 
myXor :: Bool -> Bool -> Bool
myXor x y 
 | x /= y = True
 | x == y = False

exOr :: Bool -> Bool -> Bool
exOr True True = False
exOr True False = True
exOr False False = False
exOr False True = True

prop_myXor :: Bool -> Bool -> Bool
prop_myXor x y = (exOr x y) == (myXor x y)

nAnd :: Bool -> Bool -> Bool
nAnd x y = not ( (not x) || (not y))

-- prop_nAnd :: Bool -> Bool -> Bool
-- prop_nAnd x y = nan

threeDifferent ::  Integer -> Integer -> Integer -> Bool
threeDifferent x y z = not ( (x==y) || (x==z) || (y==z))

fourEqual :: Integer -> Integer -> Integer -> Integer -> Bool
fourEqual a b c d = a==b && b==c && c==d

onThreeLines :: String -> String -> String -> String
onThreeLines a b c = a ++ "\n" ++ b ++ "\n" ++ c

-- romanDigit :: Char -> String
-- romanDigit 

averageThree :: Integer -> Integer -> Integer -> Float
averageThree a b c = (fromIntegral a + fromIntegral b + fromIntegral c) / 3  

howManyAboveAverage :: Integer -> Integer -> Integer -> Integer 
howManyAboveAverage a b c = fromIntegral $ length [x| x<-[a, b, c], fromIntegral x > avg] 
          where avg = averageThree a b c

prop_avg :: Integer -> Integer -> Integer -> Bool
prop_avg a b c = avg == fromIntegral (a + b + c) / 3.0
      where avg = averageThree a b c

-- prop_many :: Integer -> Integer -> Integer -> Integer -> Bool
-- prop_many a b c d = d == leng

numberNDroots :: Float -> Float -> Float -> Integer
numberNDroots a b c 
  | b ^ 2 > what   = 2
  | b ^ 2 == what  = 1
  | b ^ 2 < what   = 0
  where what = 4.0 * a * b

numberRoots :: Float -> Float -> Float -> Integer
numberRoots a b c
  | a /= 0 = numberNDroots a b c
  | a == 0 && b /= 0  = 1
  | a == 0 && b == 0 && c /= 0 = 0
  | a == 0 && b == 0 && c == 0  = 3


smallerRoot :: Float -> Float -> Float -> Float
smallerRoot a b c
  | discriminant > 0 = min root1 root2
  | otherwise        = 0
  where discriminant = b^2 - 4*a*c
        root1        = (-b - sqrt discriminant) / (2 * a)
        root2        = (-b + sqrt discriminant) / (2 * a)

largerRoot :: Float -> Float -> Float -> Float
largerRoot a b c
  | discriminant > 0 = max root1 root2
  | otherwise        = 0
  where discriminant = b^2 - 4*a*c
        root1        = (-b - sqrt discriminant) / (2 * a)
        root2        = (-b + sqrt discriminant) / (2 * a)

prop_large :: Float -> Float -> Float -> Bool
prop_large a b c 
  | roots == 0 || roots == 3 = lar == 0
  | roots == 1 = lar == small && test0
  | roots == 2 = lar > small && test0
  where
      lar = largerRoot a b c
      small = smallerRoot a b c
      roots = numberRoots a b c
      test0 = abs ( a * lar ^ 2 + b * lar + c) < 1e-6 
  

  
