module Main where


import Control.Exception (assert)


{- Given [1, 3, 7, 10, 25, 50], Target 765, one solution (1+50)*(25-10) -}

data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"


valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y


data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n)     = show n
  show (App o l r) = brak l ++ show o ++ brak r
    where brak (Val n) = show n
          brak e       = "(" ++ show e ++ ")"


values :: Expr -> [Int]
values (Val n)     = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]  -- singleton list denotes success, empty list denotes failure
eval (Val n)     = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]


subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
  where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices = concat . map perms . subs

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x:xs) = ([x], xs) : [(x:ls, rs) | (ls, rs) <- split xs]


tests = [
  valid Add 1 1,
  valid Sub 3 2,
  not (valid Sub 2 3),
  valid Mul 3 2,
  valid Div 4 2,
  not (valid Div 3 2),

  apply Add 1 1 == 2,
  apply Sub 3 2 == 1,
  apply Mul 3 2 == 6,
  apply Div 4 2 == 2,

  show (App Add (Val 1) (App Mul (Val 2) (Val 3))) == "1+(2*3)",

  values (App Add (Val 1) (App Mul (Val 2) (Val 3))) == [1, 2, 3],

  eval (App Add (Val 2) (Val 3)) == [5],
  eval (App Sub (Val 2) (Val 3)) == [],

  subs [1, 2, 3] == [[], [3], [2], [2,3], [1], [1,3], [1,2], [1,2,3]],
  interleave 1 [2, 3, 4] == [[1,2,3,4], [2,1,3,4], [2,3,1,4], [2,3,4,1]],
  perms [1, 2, 3] == [[1,2,3], [2,1,3], [2,3,1], [1,3,2], [3,1,2], [3,2,1]],
  choices [1, 2, 3] == [[], [3], [2], [2,3], [3,2], [1], [1,3], [3,1], [1,2], [2,1], [1,2,3], [2,1,3], [2,3,1], [1,3,2], [3,1,2], [3,2,1]],

  solution (App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10))) [1,3,7,10,25,50] 765,

  split [1, 2, 3, 4] == [([1], [2,3,4]), ([1,2], [3,4]), ([1,2,3], [4])],

  True
  ]

main :: IO ()
main = assert (and tests) putStrLn "OK"
