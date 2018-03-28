module Main where

import Control.Exception (assert)



-- 1)
{- If given a negative argument, fac will never terminate because n is
already less than the base case and will continue to decrease without limit.
Here is a guard to prevent negative arguments: -}
fac :: Int -> Int
fac 0 = 1
fac n | n > 0  = n * fac (n - 1)

{- This guard works because it doesn't *have* to define any alternative cases.
If a negative argument is given (an undefined caes), then a "non-exhaustive
pattern" exception is raised. -}



-- 2)
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)



-- 3)
{- Exponentiation is repeated multiplication, just as multiplication is
repeated additions: -}
pow :: Int -> Int -> Int
pow _ 0 = 1
pow m n = m * pow m (n - 1)

{-
pow 2 3
2 * pow 2 2
2 * 2 * pow 2 1
2 * 2 * 2 * pow 2 0
2 * 2 * 2 * 1
8
-}



-- 4)
euclid :: Int -> Int -> Int
euclid m n | m == n     = m
           | m < n      = euclid (n - m) m
           | otherwise  = euclid (m - n) n


euclid' :: Int -> Int -> Int
euclid' m n | m == n     = m
            | otherwise  = euclid' (larger - smaller) smaller
  where (smaller, larger) = if m < n then (m, n) else (n, m)



-- 5)
{-
length [1, 2, 3]
1 + length [2, 3]
1 + 1 + length [3]
1 + 1 + 1 + length []
1 + 1 + 1 + 0
3
-}

{-
drop 3 [1, 2, 3, 4, 5]
drop 2 [2, 3, 4, 5]
drop 1 [3, 4, 5]
drop 0 [4, 5]
[4, 5]
-}

{-
init [1, 2, 3]
1 : init [2, 3]
1 : 2 : init [3]
1 : 2 : []
[1, 2]
-}



-- 6a)
and' :: [Bool] -> Bool
and' []     = True
and' (x:xs) = x && and' xs


-- 6b)
concat' :: [[a]] -> [a]
concat' []       = []
concat' (xs:xss) = xs ++ concat' xss


-- 6c)
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n - 1) x


-- 6d)
get :: [a] -> Int -> a
get (x:_) 0 = x
get xs n | not (null xs)  = get (tail xs) (n - 1)  -- guard against empty list


-- 6e)
elem' :: Eq a => a -> [a] -> Bool
elem' _  []                  = False
elem' x' (x:xs) | x' == x    = True
                | otherwise  = elem' x' xs



-- 7)
{- Both lists are already sorted, so if x is less than y, then x must be less
than all other values in ys (and similarly for y with respect to xs). -}
merge :: Ord a => [a] -> [a] -> [a]
merge []     ys                  = ys
merge xs     []                  = xs
merge (x:xs) (y:ys) | x <= y     = x : merge xs (y:ys)
                    | otherwise  = y : merge (x:xs) ys



-- 8)
halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
  where n = (length xs) `div` 2


msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort xs1) (msort xs2)
  where (xs1, xs2) = halve xs



-- 9a)
sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs


-- 9b)
take' :: Int -> [a] -> [a]
take' 0 xs     = []
take' _ []     = []
take' n (x:xs) = x : take' (n - 1) xs


-- 9c)
last' :: [a] -> a
last' [x] = x
last' (x:xs) = last' xs



tests = [
  fac 10 == 3628800,

  sumdown 3 == 6,

  2 `pow` 3 == 8,

  euclid 6 27 == 3,
  euclid' 6 27 == 3,

  and' [True, True, True] == True,
  and' [True, False, True] == False,

  concat' [[1..3], [4, 5], [6..8]] == [1..8],

  replicate' 3 2 == [2, 2, 2],

  get [1..5] 2 == 3,

  elem' 3 [1..5] == True,
  elem' 9 [1..5] == False,

  merge [2, 5, 6] [1, 3, 4] == [1..6],

  halve [1..5] == ([1, 2], [3..5]),

  msort [4, 1, 3, 5, 2] == [1..5],

  sum' [1..10] == 55,

  take' 3 [1..5] == [1..3],

  last' [1..5] == 5
  ]

main :: IO ()
main = assert (and tests) putStrLn "OK"
