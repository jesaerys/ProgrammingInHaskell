module Main where

import Control.Exception (assert)



-- 1)
filterMap :: (a -> Bool) -> (a -> b) -> [a] -> [b]
filterMap p f = map f . filter p



-- 2)
all' :: (a -> Bool) -> [a] -> Bool
all' p = and . map p

any' :: (a -> Bool) -> [a] -> Bool
any' p = or . map p

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ []                  = []
takeWhile' p (x:xs) | p x        = x : takeWhile' p xs
                    | otherwise  = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ []                  = []
dropWhile' p (x:xs) | p x        = dropWhile' p xs
                    | otherwise  = x : xs



-- 3)
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x ys -> f x : ys) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x : xs else xs) []



-- 4)
-- This solution was more obvious to me at first, but it's much less elegant
-- than the book's solution
dec2Int :: [Int] -> Int
dec2Int = foldl (\t (n, x) -> t + x * 10^n) 0 . zip [0..] . reverse

-- Book's solution:
dec2Int' :: [Int] -> Int
dec2Int' = foldl (\x y -> 10*x + y) 0



-- 5)
curry' :: ((a, b) -> c) -> (a -> b -> c)
curry' f = \x -> (\y -> f (x, y))

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f = \(x, y) -> f x y



-- 6)
type Bit = Int

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x        = []
               | otherwise  = h x : unfold p h t (t x)

int2Bin :: Int -> [Bit]
int2Bin = unfold (== 0) (`mod` 2) (`div` 2)

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold null (take 8) (drop 8)

map'' :: (a -> b) -> [a] -> [b]
map'' f = unfold null (f . head) tail

iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : unfold (\x -> False) f f x



tests = [
  let p = even
      f = (*2)
      xs = [1..10]
   in filterMap p f xs == [f x | x <- xs, p x],

  all' even [2, 4, 6] == True,
  all' even [1, 4, 6] == False,
  all' even [1, 3, 5] == False,
  any' even [2, 4, 6] == True,
  any' even [1, 4, 6] == True,
  any' even [1, 3, 5] == False,
  takeWhile' even [2, 4, 6, 7, 8] == [2, 4, 6],
  dropWhile' even [2, 4, 6, 7, 8] == [7, 8],

  map' (*2) [1..5] == [2, 4, 6, 8, 10],
  filter' even [1..10] == [2, 4, 6, 8, 10],

  dec2Int [2, 3, 4, 5] == 2345,
  dec2Int' [2, 3, 4, 5] == 2345,

  let f :: (Int, Int) -> Int
      f (x, y) = x + y
   in (curry' f) 1 2 == 3,
  let f :: Int -> Int -> Int
      f x y = x + y
   in (uncurry' f) (1, 2) == 3,

  int2Bin 13 == [1, 0, 1, 1],
  chop8 [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0] == [[1,0,0,0,0,1,1,0],[0,1,0,0,0,1,1,0],[1,1,0,0,0,1,1,0]],
  map'' (*2) [1..5] == [2, 4, 6, 8, 10],
  take 5 (iterate' (*2) 2) == [2, 4, 8, 16, 32]
  ]

main :: IO ()
main = assert (and tests) putStrLn "OK"
