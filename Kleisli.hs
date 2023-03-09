module Kleisli where
import Data.Char (toUpper)

type Writer a = (a, String)

(>=>) :: (a -> Writer b) -> (b -> Writer c) -> (a -> Writer c)
m1 >=> m2 = \x ->
  let (y, s1) = m1 x
      (z, s2) = m2 y
  in (z, s1 ++ s2)

returnA :: a -> Writer a
returnA x = (x, "")

upCase :: String -> Writer String
upCase s = (map toUpper s, "Upcase: ")

toWords :: String -> Writer [String]
toWords s = (words s, "toWords:")

process :: String -> Writer [String]
process = upCase >=> toWords
