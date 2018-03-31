module Main where

import Control.Exception (assert)


add :: Int -> Int -> Int
add x y = x + y

add' :: Int -> (Int -> Int)
add' = \x -> (\y -> x + y)



tests = [
  add 4 5 == add' 4 5
  ]

main :: IO ()
main = assert (and tests) putStrLn "OK"
