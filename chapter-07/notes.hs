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


tests = [
  add 4 5 == add' 4 5,

  twice (*2) 3 == 12,
  twice reverse [1, 2, 3] == [1, 2, 3],
  quadruple 2 == 8
  ]

main :: IO ()
main = assert (and tests) putStrLn "OK"
