module Main where

import Control.Exception (assert)


add :: Int -> Int -> Int
add x y = x + y

add' :: Int -> (Int -> Int)
add' = \x -> (\y -> x + y)


twice :: (a -> a) -> a -> a
twice f x = f (f x)

quadruple :: Num a => a -> a
quadruple = twice (*2)


map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

map'' :: (a -> b) -> [a] -> [b]
map'' f []     = []
map'' f (x:xs) = f x : map'' f xs


filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = [x | x <- xs, p x]

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p []                  = []
filter'' p (x:xs) | p x        = x : filter'' p xs
                  | otherwise  = filter'' p xs


sumsqreven :: [Int] -> Int
sumsqreven xs = sum (map (^2) (filter even xs))


all' :: (a -> Bool) -> [a] -> Bool
all' p []                  = True
all' p (x:xs) | p x        = all' p xs
              | otherwise  = False


any' :: (a -> Bool) -> [a] -> Bool
any' p []                  = False
any' p (x:xs) | p x        = True
              | otherwise  = any' p xs


takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p []                  = []
takeWhile' p (x:xs) | p x        = x : takeWhile' p xs
                    | otherwise  = []


dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p []                  = []
dropWhile' p (x:xs) | p x        = dropWhile' p xs
                    | otherwise  = x : xs


tests = [
  add 4 5 == add' 4 5,

  twice (*2) 3 == 12,
  twice reverse [1, 2, 3] == [1, 2, 3],
  quadruple 2 == 8,

  map' (+1) [1, 3, 5, 7] == [2, 4, 6, 8],
  map' even [1, 2, 3, 4] == [False, True, False, True],
  map' reverse ["abc", "def", "ghi"] == ["cba", "fed", "ihg"],
  map' (map' (+1)) [[1, 2, 3], [4, 5]] == [[2, 3, 4], [5, 6]],

  map'' (+1) [1, 3, 5, 7] == [2, 4, 6, 8],

  filter' even [1..10] == [2, 4, 6, 8, 10],
  filter' (> 5) [1..10] == [6..10],
  filter' (/= ' ') "abc def ghi" == "abcdefghi",

  filter'' even [1..10] == [2, 4, 6, 8, 10],

  sumsqreven [1..10] == 220,

  all' even [2, 4, 6, 8] == True,

  any' odd [2, 4, 6, 8] == False,

  takeWhile' even [2, 4, 6, 7, 8] == [2, 4, 6],

  dropWhile' odd [1, 3, 5, 6, 7] == [6, 7]
  ]

main :: IO ()
main = assert (and tests) putStrLn "OK"
