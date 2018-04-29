module AbstractMachine where


import Control.Exception (assert)


data Expr = Val Int | Add Expr Expr

value' :: Expr -> Int
value' (Val n)   = n
value' (Add x y) = value' x + value' y


type Cont = [Op]

data Op = EVAL Expr | ADD Int

eval :: Expr -> Cont -> Int
eval (Val n)   c = exec c n
eval (Add x y) c = eval x (EVAL y : c)

exec :: Cont -> Int -> Int
exec []           n = n
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD m : c)  n = exec c (m+n)

value :: Expr -> Int
value e = eval e []

{-
value (Add (Add (Val 2) (Val 3)) (Val 4))
eval (Add (Add (Val 2) (Val 3)) (Val 4)) []
eval (Add (Val 2) (Val 3)) [EVAL (Val 4)]
eval (Val 2) [EVAL (Val 3), EVAL (Val 4)]
exec [EVAL (Val 3), EVAL (Val 4)] 2
eval (Val 3) [ADD 2, EVAL (Val 4)]
exec [ADD 2, EVAL (Val 4)] 3
exec [EVAL (Val 4)] 2+3
eval (Val 4) [ADD 5]
exec [ADD 5] 4
exec [] 5+4
9
-}


tests = [
  value' (Add (Add (Val 2) (Val 3)) (Val 4)) == 9,
  value (Add (Add (Val 2) (Val 3)) (Val 4)) == 9
  ]

main :: IO ()
main = assert (and tests) putStrLn "OK"
