module Binary where

import Data.Char

import Control.Exception (assert)


type Bit = Int


bit2Int :: [Bit] -> Int
bit2Int bits = sum [2^i * b | (b, i) <- zip bits [0..]]

bit2Int' :: [Bit] -> Int
bit2Int' bits = sum [w*b | (w, b) <- zip weights bits]
  where weights = iterate (*2) 1

bit2Int'' :: [Bit] -> Int
bit2Int'' = foldr (\x y -> x + 2 * y) 0

int2Bin :: Int -> [Bit]
int2Bin 0      = []
int2Bin x = x `mod` 2 : int2Bin (x `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2Bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 []   = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bit2Int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id


tests = [
  bit2Int [1, 0, 1, 1] == 13,
  bit2Int' [1, 0, 1, 1] == 13,
  bit2Int'' [1, 0, 1, 1] == 13,
  int2Bin 13 == [1, 0, 1, 1],
  make8 [1, 0, 1, 1] == [1, 0, 1, 1, 0, 0, 0, 0],
  encode "abc" == [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0],
  chop8 [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0] == [[1,0,0,0,0,1,1,0],[0,1,0,0,0,1,1,0],[1,1,0,0,0,1,1,0]],
  decode [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0] == "abc",
  transmit "higher-order functions are easy" == "higher-order functions are easy"
  ]

main :: IO ()
main = assert (and tests) putStrLn "OK"
