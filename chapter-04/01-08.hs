module Main where

import Control.Exception (assert)


test :: Eq a => a -> a -> IO ()
test observed expected = assert (observed == expected) putStr ""

-- test can't handle [] and I don't yet know how to dispatch on type.
test' :: [a] -> IO ()
test' xs = assert (null xs) putStr ""



-- 1)
-- Assume the argument is always even in length.

-- take and drop:
halveA :: [a] -> ([a], [a])
halveA xs = (take n xs, drop n xs)
  where n = length xs `div` 2


-- splitAt:
halveB :: [a] -> ([a], [a])
halveB xs = splitAt n xs
  where n = length xs `div` 2



-- 2)
-- Assume the argument has at least 3 elements.

-- head and tail:
thirdA :: [a] -> a
thirdA xs = head (tail (tail xs))  -- Or,  head $ tail $ tail xs


-- List indexing:
thirdB :: [a] -> a
thirdB = (!! 2)  -- This is called a right section. Could also use an explicit arg:  thirdB xs = xs !! 2


-- Pattern matching:
-- _ is a wildcard for anything, so it works in both the head position
-- and in the tail position.
thirdC :: [a] -> a
thirdC (_:_:x:_) = x



-- 3)

-- Conditional expression:
safetailA :: [a] -> [a]
safetailA xs = if (null xs) then [] else (tail xs)


-- Guarded equations:
safetailB :: [a] -> [a]
safetailB xs | null xs    = []
             | otherwise  = tail xs


-- Pattern matching:
safetailC :: [a] -> [a]
safetailC [] = []
safetailC xs = tail xs



-- 4)
disjunction :: Bool -> Bool -> Bool
disjunction True  True  = True
disjunction True  False = True
disjunction False True  = True
disjunction False False = False

-- or,
--(||) :: Bool -> Bool -> Bool
--True  (||) True  = True
--True  (||) False = True
--False (||) True  = True
--False (||) False = False



-- 5)
conjunction :: Bool -> Bool -> Bool
conjunction x y = if x then (if y then True else False) else False



-- 6)
conjunction' :: Bool -> Bool -> Bool
conjunction' x y = if x then y else False



-- 7)
mult :: Int -> (Int -> (Int -> Int))
mult = \x -> (\y -> (\z -> x * y * z))



-- 8)
luhnDouble :: Int -> Int
luhnDouble x | y > 9      = y - 9
             | otherwise  = y
  where y = 2 * x

-- or,
luhnDouble' :: Int -> Int
luhnDouble' x = if y > 9 then y - 9 else y
  where y = 2 * x


luhn :: Int -> Int -> Int -> Int -> Bool
luhn x1 x2 x3 x4 = divisibleBy10 (sum digits)
  where digits = [luhnDouble x1, x2, luhnDouble x3, x4]
        divisibleBy10 x = (x `mod` 10) == 0

-- or,
luhn' :: Int -> Int -> Int -> Int -> Bool
luhn' x1 x2 x3 x4 = (== 0) $ (`mod` 10) $ sum digits
  where digits = [luhnDouble x1, x2, luhnDouble x3, x4]

-- or,
luhn'' :: Int -> Int -> Int -> Int -> Bool
luhn'' x1 x2 x3 x4 = total `mod` 10 == 0
  where total = sum [luhnDouble x1, x2, luhnDouble x3, x4]

-- or,
luhn''' :: Int -> Int -> Int -> Int -> Bool
luhn''' x1 x2 x3 x4 = divisibleBy10 total
  where total = sum [luhnDouble x1, x2, luhnDouble x3, x4]
        divisibleBy10 x = (x `mod` 10) == 0


main :: IO ()
main = do test (halveA [1..6]) ([1, 2, 3], [4, 5, 6])
          test (halveB [1..6]) ([1, 2, 3], [4, 5, 6])

          test (thirdA [1..5]) 3
          test (thirdB [1..5]) 3
          test (thirdC [1..5]) 3

          test (safetailA [1, 2, 3]) [2, 3]
          test' (safetailA [])
          test (safetailB [1, 2, 3]) [2, 3]
          test' (safetailB [])
          test (safetailC [1, 2, 3]) [2, 3]
          test' (safetailC [])

          test (disjunction True  True ) True
          test (disjunction True  False) True
          test (disjunction False True ) True
          test (disjunction False False) False

          test (conjunction True  True ) True
          test (conjunction True  False) False
          test (conjunction False True ) False
          test (conjunction False False) False

          test (conjunction' True  True ) True
          test (conjunction' True  False) False
          test (conjunction' False True ) False
          test (conjunction' False False) False

          test (mult 2 3 4) 24

          test (luhnDouble 3) 6
          test (luhnDouble 6) 3
          test (luhnDouble' 3) 6
          test (luhnDouble' 6) 3

          test (luhn 1 7 8 4) True
          test (luhn 4 7 8 3) False
          test (luhn' 1 7 8 4) True
          test (luhn' 4 7 8 3) False
          test (luhn'' 1 7 8 4) True
          test (luhn'' 4 7 8 3) False
          test (luhn''' 1 7 8 4) True
          test (luhn''' 4 7 8 3) False
