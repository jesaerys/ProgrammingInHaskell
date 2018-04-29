module Main where

import Control.Exception (assert)
import Data.List (nub)



data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          | Equiv Prop Prop
          | Imply Prop Prop
  deriving Show

type Subst = Assoc Char Bool


isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find' x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Or p q)    = eval s p || eval s q
eval s (Equiv p q) = eval s p == eval s q
eval s (Imply p q) = eval s p <= eval s q

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
  where vs = nub (vars p)

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
  where bss = bools (n-1)

vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Or p q)    = vars p ++ vars q
vars (Equiv p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q



type Assoc k v = [(k, v)]

find' :: Eq k => k -> Assoc k v -> v
find' k t = head [v | (k', v) <- t, k' == k]



p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')


tests = [
  eval [] (Const True),
  not (eval [] (Const False)),

  eval [('A', True),  ('B', False)] (Var 'A'),
  not (eval [('A', False), ('B', True)]  (Var 'A')),
  eval [('A', False), ('B', True)]  (Var 'B'),
  not (eval [('A', True),  ('B', False)] (Var 'B')),

  not (eval [] ((Not (Const True)))),
  eval [] ((Not (Const False))),

  eval [] (And (Const True)  (Const True)),
  not (eval [] (And (Const True)  (Const False))),
  not (eval [] (And (Const False) (Const True))),
  not (eval [] (And (Const False) (Const False))),

  eval [] (Or (Const True)  (Const True)),
  eval [] (Or (Const True)  (Const False)),
  eval [] (Or (Const False) (Const True)),
  not (eval [] (Or (Const False) (Const False))),

  eval [] (Equiv (Const True)  (Const True)),
  not (eval [] (Equiv (Const True)  (Const False))),
  not (eval [] (Equiv (Const False) (Const True))),
  eval [] (Equiv (Const False) (Const False)),

  eval [] (Imply (Const True)  (Const True)),
  not (eval [] (Imply (Const True)  (Const False))),
  eval [] (Imply (Const False) (Const True)),
  eval [] (Imply (Const False) (Const False)),

  vars p1 == "AA",
  vars p2 == "ABA",
  vars p3 == "AAB",
  vars p4 == "AABB",

  bools 3 == [[False, False, False],
              [False, False, True ],
              [False, True,  False],
              [False, True,  True ],
              [True , False, False],
              [True , False, True ],
              [True , True,  False],
              [True , True,  True ]],

  substs p1 == [[('A', False)],
                [('A', True )]],
  substs p2 == [[('A', False), ('B', False)],
                [('A', False), ('B', True )],
                [('A', True ), ('B', False)],
                [('A', True ), ('B', True )]],
  substs p3 == [[('A', False), ('B', False)],
                [('A', False), ('B', True )],
                [('A', True ), ('B', False)],
                [('A', True ), ('B', True )]],
  substs p4 == [[('A', False), ('B', False)],
                [('A', False), ('B', True )],
                [('A', True ), ('B', False)],
                [('A', True ), ('B', True )]],

  not (isTaut p1),
  isTaut p2,
  not (isTaut p3),
  isTaut p4,
  isTaut (Equiv (Or (Var 'A') (Const True)) (Const True))
  ]

main :: IO ()
main = assert (and tests) putStrLn "OK"
