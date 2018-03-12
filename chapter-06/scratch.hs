module Main where

import Control.Exception (assert)


fac :: Int -> Int
fac n = if n == 0 then 1 else n * fac (n - 1)

fac' :: Int -> Int
fac' n | n == 0     = 1
       | otherwise  = n * fac' (n - 1)

fac'' :: Int -> Int
fac'' 0 = 1
fac'' n = n * fac'' (n - 1)


mult :: Int -> Int -> Int
mult m 0 = 0
mult m n = m + (mult m (n - 1))


product' :: Num a => [a] -> a
product' []     = 1
product' (x:xs) = x * product' xs


length' :: [a] -> Int
length' []     = 0
length' (_:xs) = 1 + length' xs


reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]


append :: [a] -> [a] -> [a]
append [] ys     = ys
append (x:xs) ys = x : (append xs ys)


insert :: Ord a => a -> [a] -> [a]
insert x []                  = [x]
insert x (y:ys) | x <= y     = x : y : ys
                | otherwise  = y : insert x ys


isort :: Ord a => [a] -> [a]
isort []     = []
isort (x:xs) = insert x (isort xs)


tests = [
  fac 10 == 3628800,
  fac' 10 == 3628800,
  fac'' 10 == 3628800,

  mult 3 6 == 18,

  product' [1, 2, 3, 4, 5] == 120,

  length' [1, 2, 3, 4, 5] == 5,

  reverse' [1, 2, 3, 4, 5] == [5, 4, 3, 2, 1],

  append [1, 2, 3] [4, 5, 6] == [1, 2, 3, 4, 5, 6],

  insert 3 [1, 2, 3, 4, 5] == [1, 2, 3, 3, 4, 5],

  isort [4, 1, 3, 5, 2] == [1, 2, 3, 4, 5]
  ]

main :: IO ()
main = assert (and tests) putStrLn "OK"
