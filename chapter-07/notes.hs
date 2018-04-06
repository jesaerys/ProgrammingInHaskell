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


{-

`foldr` (fold right) encapsulates the following pattern:

    f []     = v
    f (x:xs) = x # f xs

where `v` and `#` are parameterizations of the base case value and an operator,
respectively.

-}

sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

sum'' :: Num a => [a] -> a
sum'' = foldr (+) 0

product' :: Num a => [a] -> a
product' []     = 1
product' (x:xs) = x * product' xs

product'' :: Num a => [a] -> a
product'' = foldr (*) 1

or' :: [Bool] -> Bool
or' []     = False
or' (x:xs) = x || or' xs

or'' :: [Bool] -> Bool
or'' = foldr (||) False

and' :: [Bool] -> Bool
and' []     = True
and' (x:xs) = x && and' xs

and'' :: [Bool] -> Bool
and'' = foldr (&&) True


foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v []     = v
foldr' f v (x:xs) = foldr' f (f x v) xs

foldr'' :: (a -> b -> b) -> b -> [a] -> b
foldr'' f v []     = v
foldr'' f v (x:xs) = f x (foldr'' f v xs)


{-

`foldr` can be thought of as replacing cons with the function and the empty
list with the value. For example,

    foldr (+) 0 (1 : (2 : (3 : [])))

is the same as,

    1 + (2 + (3 + 0))

-}


length' :: [a] -> Int
length' []     = 0
length' (x:xs) = 1 + length' xs

length'' :: [a] -> Int
length'' = foldr (\_ n -> n + 1) 0


snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = snoc x (reverse' xs)

reverse'' :: [a] -> [a]
reverse'' = foldr snoc []


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

  dropWhile' odd [1, 3, 5, 6, 7] == [6, 7],

  sum' [1..10] == 55,
  sum'' [1..10] == 55,

  product' [1..10] == 3628800,
  product'' [1..10] == 3628800,

  or' [True, True, True] == True,
  or'' [True, True, True] == True,
  or' [True, False, True] == True,
  or'' [True, False, True] == True,
  or' [False, False, False] == False,
  or'' [False, False, False] == False,

  and' [True, True, True] == True,
  and'' [True, True, True] == True,
  and' [True, False, True] == False,
  and'' [True, False, True] == False,
  and' [False, False, False] == False,
  and'' [False, False, False] == False,

  foldr' (+) 0 [1..10] == 55,
  foldr'' (+) 0 [1..10] == 55,

  length' [1..10] == 10,
  length'' [1..10] == 10,

  snoc 1 [2] == [2, 1],
  reverse' [1, 2, 3, 4] == [4, 3, 2, 1],
  reverse'' [1, 2, 3, 4] == [4, 3, 2, 1]
  ]

main :: IO ()
main = assert (and tests) putStrLn "OK"
