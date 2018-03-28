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



tests = [
  fac 10 == 3628800,

  sumdown 3 == 6,

  2 `pow` 3 == 8,

  euclid 6 27 == 3,
  euclid' 6 27 == 3,

  and' [True, True, True] == True,
  and' [True, False, True] == False,

  concat' [[1, 2, 3], [4, 5], [6, 7, 8]] == [1,2,3,4,5,6,7,8],

  replicate' 3 2 == [2, 2, 2],

  get [1, 2, 3, 4, 5] 2 == 3,

  elem' 3 [1, 2, 3, 4, 5] == True,
  elem' 9 [1, 2, 3, 4, 5] == False
  ]

main :: IO ()
main = assert (and tests) putStrLn "OK"
