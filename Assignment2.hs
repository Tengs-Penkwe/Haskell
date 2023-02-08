module Assignment2 where
{- Question 1a -}

tails [] = [[]]
tails (e:r) = (e:r):tails(r)

{- 
  i)  [a] -> [[a]]
 ii)  ["happy", "appy", "ppy", 'py', "y", ""]
iii)  [[Char]]
 - -}

-- tails1 xs = foldr func [[]] xs 
--   where func x acc = x:xs : acc
--
--   it's impossible, because if we want to get a list of list, we need to 
--   traverse the list multiple times, but foldr and foldl only travser one time.

{- Question 1b-}
doif f g [] = []
doif f g (h:t)
  | f h = g h : doif f g t
  | otherwise = h : doif f g t

{-
 - -}




