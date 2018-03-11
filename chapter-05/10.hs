module CaesarCipher where

import Control.Exception (assert)
import Data.Char (ord, chr, isLower)


-- | Decode a message that was encoded using the Caesar cipher.
--
-- This works by determining the cipher's shift parameter that leads to
-- character frequencies that are most similar to clear text. It may fail when
-- the message is short or when the message has an unusual character
-- distribution.
crack :: String -> String
crack xs = encode (-factor) xs
  where factor = head (positions (minimum chitab) chitab)
        chitab = [chisqr (rotate n table') table | n <- [0..25]]
        table' = freqs xs


-- | Encode a string using the Caesar cipher.
encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]


-- | Alphabetically increment a lowercase character, wrapping around after "z".
-- All other characters are unchanged.
shift :: Int -> Char -> Char
shift n c | isLower c  = int2let ((let2int c + n) `mod` 26)
          | otherwise  = c


-- | Compute the character frequency table of a string.
--
-- Only lowercase characters a--z are considered.
freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
  where n = lowers xs


-- | Reference table of empirical character frequencies in the English
-- language. Frequencies are given as percentages.
table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2,
         0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.0,
         2.8, 1.0, 2.4, 0.2, 2.0, 0.1]


testCaesarCipher :: Bool
testCaesarCipher = and [
  crack "kdvnhoo lv ixq" == "haskell is fun",
  crack "vscd mywzboroxcsyxc kbo ecopev" == "list comprehensions are useful",

  encode 3 "haskell is fun" == "kdvnhoo lv ixq",
  encode (-3) "kdvnhoo lv ixq" == "haskell is fun",

  shift 3 'a' == 'd',
  shift 3 'z' == 'c',
  shift (-3) 'c' == 'z',
  shift 3 ' ' == ' ',

  freqs "abbcccddddeeeee" == [6.666667, 13.333334, 20.0, 26.666668, 33.333336, 0.0, 0.0, 0.0, 0.0, 0.0,
                              0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
                              0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
  ]


tests = [
  testCaesarCipher,
  testCharacterUtilities,
  testStringUtilities,
  testMathUtilities,
  testListUtilities
  ]

test :: IO ()
test = assert (and tests) putStrLn "OK"



-- Note: the remainder of this script contains functions that I'd rather
-- extract into their own modules, but I don't know how to do that yet.



-- Character utilities ---------------------------------------------------------

-- | Convert a character to its numeric representation relative to "a".
let2int :: Char -> Int
let2int c = ord c - ord 'a'


-- | Inverse of 'let2int'.
int2let :: Int -> Char
int2let n = chr (ord 'a' + n)


testCharacterUtilities :: Bool
testCharacterUtilities = and [
  let2int 'a' == 0,
  int2let 0 == 'a'
  ]



-- String utilities ------------------------------------------------------------

-- | Get the positions of an element within a list of elements.
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x' == x]


-- | Count the number of lowercase characters in a string.
lowers :: String -> Int
lowers xs = length [x | x <- xs, isLower x]


-- | Count the occurrences of a character in a string.
count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x' == x]


testStringUtilities :: Bool
testStringUtilities = and [
  positions False [True, False, True, False] == [1, 3],
  lowers "Haskell" == 6,
  count 's' "Mississippi" == 4
  ]



-- Math utilities --------------------------------------------------------------

-- | Chi-squared statistic for an observed frequency distribution relative to
-- an expected frequency distribution.
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [(o - e)^2 / e | (o, e) <- zip os es]


-- | Percent of a number with respect to another number, normalized to 100.
percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100


testMathUtilities :: Bool
testMathUtilities = and [
  chisqr [4, 7, 5, 6, 3] [5, 5, 5, 5, 5] == 2.0,
  percent 3 12 == 25.0
  ]



-- List utilities --------------------------------------------------------------

-- | Rotate elements to the left such that the head wraps to the end.
rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs


testListUtilities :: Bool
testListUtilities = and [
  rotate 3 [1..5] == [4, 5, 1, 2, 3]
  ]
