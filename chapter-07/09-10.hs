module Main where

import Control.Exception (assert)


luhn :: [Int] -> Bool
luhn = (== 0) . (`mod` 10) . sum . reverse . (altMap id luhnDouble) . reverse

luhnDouble :: Int -> Int
luhnDouble x | y > 9      = y - 9
             | otherwise  = y
  where y = 2 * x

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g = map (\(h, x) -> h x) . zip (cycle [f, g])


tests = [
  altMap (+10) (+100) [0, 1, 2, 3, 4] == [10, 101, 12, 103, 14],

  luhnDouble 3 == 6,
  luhnDouble 6 == 3,

  luhn [1, 7, 8, 4] == True,
  luhn [4, 7, 8, 3] == False,
  luhn [1,8,6,2, 2,0,4,6, 2,0,3,0, 0,0,2,3] == True,
  luhn [1,8,6,2, 2,0,4,6, 2,0,3,0, 0,0,2,4] == False
  ]

main :: IO ()
main = assert (and tests) putStrLn "OK"
