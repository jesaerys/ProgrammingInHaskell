module Main where

import Control.Exception (assert)



-- 1)
sumsq100 :: Int
sumsq100 = sum [x^2 | x <- [1..100]]



-- 2)
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0..m], y <- [0..n]]



-- 3)
square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]



-- 4)
replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n]]



-- 5)
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]



-- 6)
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]


perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (init (factors x)) == x]



-- 7)
tuples :: [(Int, Int)]
tuples = concat [[(x, y) | y <- [3, 4]] | x <- [1, 2]]



-- 8)
find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k' == k]


positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..])
-- The original implementation combines the 'find' and 'zip' logic in a single
-- comprehension:
-- > positions x xs = [i | (x', i) <- zip xs [0..], x' == x]



-- 9)
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]



tests = [
  sumsq100 == 338350,
  grid 1 2 == [(0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2)],
  square 2 == [(0, 1), (0, 2), (1, 0), (1, 2), (2, 0), (2, 1)],
  replicate' 3 True == [True, True, True],
  pyths 10 == [(3, 4, 5), (4, 3, 5), (6, 8, 10), (8, 6, 10)],
  factors 6 == [1, 2, 3, 6],
  perfects 500 == [6, 28, 496],
  tuples == [(x, y) | x <- [1, 2], y <- [3, 4]],
  find 'b' [('a', 1), ('b', 2), ('c', 3), ('b', 4)] == [2, 4],
  positions False [True, False, True, False] == [1, 3],
  scalarproduct [1, 2, 3] [4, 5, 6] == 32
  ]

main :: IO ()
main = assert (and tests) putStrLn "OK"
