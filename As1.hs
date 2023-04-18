module As1 where

harmonic :: (Integral n) => n -> Double
harmonic n 
  = sum [ 1/fromIntegral x | x<-[1..n] ]

-- winSeries :: (Real a, Integral b) a -> b -> Double
-- winSeries probabilty series
wins :: Double -> Int -> Int -> Double
wins _ 0 _ = 1
wins _ _ 0 = 0
wins p nw nl = p*(wins p (nw-1) nl) + (1-p)*(wins p nw (nl-1))

-- probable 
-- probable p s w l 
--   | s == 1  &&  w == 0 &&  l == 0 = p
--   | s >  1  && 

myreplace :: Eq a => a -> a -> [a] -> [a]
myreplace _ _ [] = []
myreplace org rpl (h:t)
  | org == h  = rpl : tails t
  | otherwise = h   : tails t
  where tails = myreplace org rpl 

-- myapply :: Eq a => [a] -> [(a,a)] -> [a]
-- myapply []  _  = []
-- myapply lst [] = lst
-- myapply lst ((org, rpl):t)
--   = myapply lst' t
--   where lst' = myreplace org rpl lst 
  
myapply :: Eq a => [a] -> [(a,a)] -> [a]
myapply []  _  = []
myapply lst [] = lst
myapply (h:t) sub
  = case lookup h sub of 
  Just x  -> x : myapply t sub
  Nothing -> h : myapply t sub

myordered :: Ord a => [a] -> Bool
myordered [] = True
myordered [_] = True
myordered (h:m:left) 
  = h <= m && myordered(m:left)

-- myremoveduplicates :: Eq a => [a] -> [a]
-- myremoveduplicates [] = []
-- -- myremoveduplicates [_] = [_]
-- myremoveduplicates (h:t) 
--   | reduce t h  = myremoveduplicates t
--   | otherwise   = h : myremoveduplicates t

-- reduce [] element = False
-- reduce (h:t) element
--   = h == element || reduce t element

myremoveduplicates :: Eq a => [a] -> [a]
myremoveduplicates = go []
  where
    go _ [] = []
    go seen (x:xs)
      | x `elem` seen = go seen xs
      | otherwise     = x : go (seen ++ [x]) xs

myremoveduplicates' :: Eq a => [a] -> [a]
myremoveduplicates' = go []
  where
    go _ [] = []
    go seen (x:xs)
      | x `elem` seen = go seen xs
      | otherwise     = x : go (seen ++ [x]) xs

deln :: Eq a => Int -> a -> [a] -> [a]
deln 0 _ lst = lst
deln _ _ [] = []
deln n e (h:t)
  | n <= 0 = h:t
  | e == h = deln (n-1) e t 
  | otherwise = h : deln n e t

delna :: Eq a => Int -> a -> [a] -> [[a]]
delna 0 _ lst = [lst]
delna _ _ []  = []
delna  n e (h:t)
    | e==h = (delna (n-1) e t) ++ cons_to_each h (delna n e t)
    | otherwise = cons_to_each h (delna n e t)
        where
            -- cons_to_each e lst  -- adds e to the front of every element of lst
            -- note that adding an element to the front of a list is often called "cons" from the Lisp function
            cons_to_each _ [] = []
            cons_to_each e (h:t) = (e:h):cons_to_each e t
-- delna n e (h:t)
--   | e == h  = (delna (n-1) e t) ++ cons_to_each h (delna n e t)
--   | 
--   | h == e    = map (h :) (delna (n-1) e t) ++ delna n e t
--   | otherwise = map (h :) (delna n e t)



