{- To run,

```
$ ghc -O2 countdown.hs
$ # Or with stack,
$ stack ghc -- -O2 countdown.hs
$ time ./countdown > solutions.txt

real	0m5.561s
user	0m5.473s
sys	0m0.073s
$ tr ',' '\n' <solutions.txt | head -n 3
[3*((7*(50-10))-25)
((7*(50-10))-25)*3
3*(((50-10)*7)-25)
```

The above result was for `solutions`. The result for `solutions'` was,

```
$ time ./countdown > solutions2.txt

real	0m0.381s
user	0m0.352s
sys	0m0.010s
$ diff solutions.txt solutions2.txt
```

This is about 15x faster.

-}
module Main where


import Control.Exception (assert)


main :: IO ()
main = print (solutions' [1, 3, 7, 10, 25, 50] 765)


data Op = Add | Sub | Mul | Div
  deriving Eq

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
  deriving Eq

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

exprs :: [Int] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns  = [e | (ls, rs) <- split ns,
                 l        <- exprs ls,
                 r        <- exprs rs,
                 e        <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]


type Result = (Expr, Int)

results :: [Int] -> [Result]
results []  = []
results [n] = [(Val n, n) | n > 0]
results ns  = [res | (ls, rs) <- split ns,
                     lx       <- results ls,
                     ry       <- results rs,
                     res      <- combine' lx ry]

combine' :: Result -> Result -> [Result]
combine' (l, x) (r, y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e, m) <- results ns', m == n]



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

  combine (Val 1) (Val 2) == [App Add (Val 1) (Val 2), App Sub (Val 1) (Val 2), App Mul (Val 1) (Val 2), App Div (Val 1) (Val 2)],
  exprs [1, 2] == [App Add (Val 1) (Val 2), App Sub (Val 1) (Val 2), App Mul (Val 1) (Val 2), App Div (Val 1) (Val 2)],
  solutions [1, 2] 2 == [Val 2, App Mul (Val 1) (Val 2), App Mul (Val 2) (Val 1), App Div (Val 2) (Val 1)],

  combine' (Val 1, 1) (Val 2, 2) == [(App Add (Val 1) (Val 2), 3), (App Mul (Val 1) (Val 2), 2)],
  results [1, 2] == [(App Add (Val 1) (Val 2), 3), (App Mul (Val 1) (Val 2), 2)],
  solutions' [1, 2] 2 == [Val 2, App Mul (Val 1) (Val 2), App Mul (Val 2) (Val 1), App Div (Val 2) (Val 1)],

  True
  ]

test :: IO ()
test = assert (and tests) putStrLn "OK"
