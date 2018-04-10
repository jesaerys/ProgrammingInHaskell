module Main where

import Data.Char

import Control.Exception (assert)


type Bit = Int


encode :: String -> [Bit]
encode = concat . map (addParityBit . make8 . int2Bin . ord)

decode :: [Bit] -> String
decode = map (chr . bit2Int . stripParityBit) . chop9


int2Bin :: Int -> [Bit]
int2Bin 0      = []
int2Bin x = x `mod` 2 : int2Bin (x `div` 2)

bit2Int :: [Bit] -> Int
bit2Int = foldr (\x y -> x + 2 * y) 0


make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

chop9 :: [Bit] -> [[Bit]]
chop9 []   = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)


addParityBit :: [Bit] -> [Bit]
addParityBit bits = bits ++ [parityBit bits]

stripParityBit :: [Bit] -> [Bit]
stripParityBit bits | (parityBit $ init bits) == last bits  = init bits
                    | otherwise                             = error "parity bit error"

parityBit :: [Bit] -> Bit
parityBit bits = if (odd . length . filter (== 1) $ bits) then 1 else 0


transmit :: ([Bit] -> [Bit]) -> String -> String
transmit channel = decode . channel . encode

perfectChannel :: [Bit] -> [Bit]
perfectChannel = id

faultyChannel :: [Bit] -> [Bit]
faultyChannel = tail


tests = [
  make8 [1, 0, 1, 1] == [1, 0, 1, 1, 0, 0, 0, 0],

  chop9 [1,0,0,0,0,1,1,0,1,0,1,0,0,0,1,1,0,1,1,1,0,0,0,1,1,0,0] == [[1,0,0,0,0,1,1,0,1],[0,1,0,0,0,1,1,0,1],[1,1,0,0,0,1,1,0,0]],

  bit2Int [1, 0, 1, 1] == 13,

  int2Bin 13 == [1, 0, 1, 1],

  addParityBit [1, 0, 1, 1, 0, 0, 0, 0] == [1, 0, 1, 1, 0, 0, 0, 0, 1],
  addParityBit [1, 0, 1, 1, 0, 0, 0, 1] == [1, 0, 1, 1, 0, 0, 0, 1, 0],

  stripParityBit [1, 0, 1, 1, 0, 0, 0, 0, 1] == [1, 0, 1, 1, 0, 0, 0, 0],
  stripParityBit [1, 0, 1, 1, 0, 0, 0, 1, 0] == [1, 0, 1, 1, 0, 0, 0, 1],
  -- To my current knowledge, there's no straightforward way to test that this
  -- function raises an exception when the parity bit is incorrect.

  encode "abc" == [1,0,0,0,0,1,1,0,1,0,1,0,0,0,1,1,0,1,1,1,0,0,0,1,1,0,0],

  decode (encode "abc") == "abc",

  transmit perfectChannel "higher-order functions are easy" == "higher-order functions are easy"

  -- This will result in a "parity bit error":
  --transmit faultyChannel "higher-order functions are easy"
  ]

main :: IO ()
main = assert (and tests) putStrLn "OK"
