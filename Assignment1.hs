module Assignment1 where
{- Question 1 -}

-- harmonic :: Integer -> Double
harmonic :: (Integral n) => n -> Double
harmonic n = sum [1/fromIntegral x | x <-[1..n]]
-- func n = [x | x <- [1..n]] is a list comprehension
--

{- Explanation 
 - `harmonic (length [1,2,3,4])`
 - My original one does not work with it, since the result of 
 - length [1,2,3,4] is a `Int`, not `Integer`, they are of different types.
 -
 - I change `Intger` to `(Integral n)`, which restrict the type of the argument
 - n to be any type that belongs to the Integral type class.
 - -}


{- Question 2 -}
winSeries :: (Real a, Integral b) => a -> b -> a
winSeries chance series = probability chance series 0 0

probability :: (Real a, Integral b) => a -> b -> b -> b -> a
probability p s w l  
  | s == 1 && w == 0 && l == 0 = p                                                              -- base case
  | s >  1 && w == 0 && l == 0 = p * triple s     1     0     + (1-p) * triple s     0     1    -- start states
  | s >  1 && w >= 1 && l >= 1 =     triple (s-2) (w-1) (l-1)                                   -- win and loss cancel out
  | s >  1 && w == 0 && l == d = p * triple (s-2) 0     (l-1)                                   -- one step to lose
  | s >  1 && w == d && l == 0 = p                            + (1-p) * triple (s-2) (w-1) 0    -- one step to win
  | s >  1 && w == 0 && l >= 1 = p * triple (s-2) 0     (l-1) + (1-p) * triple  s     0   (l+1) -- win cancels loss or win accumulates
  | s >  1 && w >= 1 && l == 0 = p * triple  s   (w+1)   0    + (1-p) * triple (s-2) (w-1) 0    -- loss cancels win or loss accumulates
  where
    triple = probability p   -- easier to iterate
    d = s `div` 2            -- one step to win or loss

-- There are three states: Series number(S), Win times(W), Lost times(L)
-- It's a Directed Acyclic Graph, each direction has its weight (chance to win or loss)
-- the function above find the probability from (S, W, L) to (1, 1, 0) 
-- base case P(S=1,W=0,L=0) = p; 
-- P(S,0,0) = p*P(S  ,1  ,0)  + (1-p)*P(S,0,1)
-- P(S,0,L) = p*P(S-2, 0 ,E)  + (1-p)*P( S,  0 ,L+1)
-- P(S,W,0) = p*P(S  ,W+1,0)  + (1-p)*P(S-2,W-1 ,0)
-- When L or W = s/2, ready to Lose or Win.
--

{- 
1. If a team has a 0.6 chance of winning each game, probability of it winning a best-of-5 series 
   > winSeries 0.6 5
   0.68256
2.If a team has a 0.6 chance of winning each game, probability of it winning a best-of-7 series 
  > winSeries 0.6 7
  0.710208
3.If each team has a 0.5 chance of winning each game, what is the probability of a team that is up 2 games to 1 in best-of-7 series will win the series?
  > probability 0.5 7 2 1
  0.6875
-}

foo :: Int -> Int
foo x = x+3
dfoo x = foo (foo x)
nfoo x = x+3+3
fooeach [] = []
fooeach (h:t) = foo h : t
nfooeach [] = []
nfooeach (h:t) = nfoo h : t
iffoo [] = []
iffoo (h:t)
  | h < 4.3 = h:t
  | otherwise = t
dd x y = 2*x + 3*y + 7

{- Question 3 -}
{-
(a) dfoo :: Int -> Int
(b) nfoo :: Num a => a -> a
(c) fooeach :: [Int] -> [Int]
(d) nfooeach :: Num a => [a] -> [a]
(e) iffoo :: (Ord a, Fractional a) => [a] -> [a]
(f) dd :: Num a => a -> a -> a
(g) dd 4.3 :: Fractional a => a -> a
 - -}

{- Question 4 -}
myreplace :: Eq a => a -> a -> [a] -> [a]
myreplace org rpl (h:t)
  | h == org    = rpl : myreplace org rpl t
  | otherwise   = h   : myreplace org rpl t

-- myapply :: Eq a => [a] -> [(a,a)] -> [a]
-- myapply lst sub = map (\x -> )

myordered :: Ord a => [a] -> Bool
myordered (f:s:left) = f <= s && myordered(s:left)
myordered [] = True
myordered [_] = True

myremoveduplicatesFirst :: Eq a => [a] -> [a]
myremoveduplicatesFirst (h:t) = h : myremoveduplicatesFirst (filter (/= h) t)
myremoveduplicatesFirst [] = []


-- myremoveduplicatesLast :: Eq a => [a] -> [a]
-- myremoveduplicatesLast (h:t) = myremoveduplicatesLast (filter (/= h) t) 
-- myremoveduplicatesLast [] = []

deln :: (Integral a, Eq b) => a -> b -> [b] -> [b]
deln n e (h:t)
  | n <= 0 = h:t
  | h == e = deln (n-1) e t
  | otherwise = h : deln n e t
deln _ _ [] = []

-- delna :: (Integral a, Eq b) => a -> b -> [b] -> [[b]]
-- delna n e (h:t)
--   | n <=0 = h:t
--   | 



{- Question 5 
1. about 5 minutes, I reviewed type constraint. too simple
2. about 2 hours, the original one was procdure-oriented, ugly and didn't work
   I re-thought this question, figured out how to solve it by recursion
   it's hard but rewarding, 
3. about 10 minutes, resonable
4. 20 + 10 + 5 + 5 minutes
 - -}
