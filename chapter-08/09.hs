module AbstractMachine where


import Control.Exception (assert)


data Expr = Val Int | Add Expr Expr | Mult Expr Expr
  deriving Show

type Cont = [Op]

data Op = EVALADD Expr | ADD Int
        | EVALMULT Expr | MULT Int
  deriving Show


eval :: Expr -> Cont -> Int
eval (Val n)    c = exec c n
eval (Add x y)  c = eval x (EVALADD y : c)
eval (Mult x y) c = eval x (EVALMULT y : c)

exec :: Cont -> Int -> Int
exec []           n = n
exec (EVALADD y : c) n = eval y (ADD n : c)
exec (EVALMULT y : c) n = eval y (MULT n : c)
exec (ADD m : c)  n = exec c (m+n)
exec (MULT m : c) n = exec c (m*n)

value :: Expr -> Int
value e = eval e []

{-
value (Add (Mult (Val 2) (Val 3)) (Val 4))
eval (Add (Mult (Val 2) (Val 3)) (Val 4)) []
eval (Mult (Val 2) (Val 3)) [EVALADD (Val 4)]
eval (Val 2) [EVALMULT (Val 3), EVALADD (Val 4)]
exec [EVALMULT (Val 3), EVALADD (Val 4)] 2
eval (Val 3) [MULT 2, EVAL (Val 4)]
exec [MULT 2, EVAL (Val 4)] 3
exec [EVALADD (Val 4)] 2*3
eval (Val 4) [ADD 6]
exec [ADD 6] 4
exec [] 6+4
10
-}


tests = [
  value (Add (Mult (Val 2) (Val 3)) (Val 4)) == 10
  ]

main :: IO ()
main = assert (and tests) putStrLn "OK"
