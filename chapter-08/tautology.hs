module Tautology where

import Control.Exception (assert)


{-

Propositions:

  1. A ∧ ¬A
  2. (A ∧ B) ⇒ A
  3. A ⇒ (A ∧ B)
  4. (A ∧ (A ⇒ B)) ⇒ B

Truth tables:

     ^a  ^b
  A  ¬A  A ∧ ¬A
  -  --  ------
  T  F   F
  F  T   F

        ^c     ^d     ^e     ^f      ^g     ^h           ^i           ^j
  A  B  A ∧ B  A v B  A ⇒ B  ¬A ∨ B  A ≤ B  (A ∧ B) ⇒ A  A ⇒ (A ∧ B)  (A ∧ (A ⇒ B)) ⇒ B
  -  -  -----  -----  -----  ------  -----  -----------  -----------  -----------------
  T  T  T      T      T      T       T      T            T            T
  T  F  F      T      F      F       F      T            F            T
  F  T  F      T      T      T       T      T            T            T
  F  F  F      F      T      T       T      T            T            T

^a Not primitive
^b Proposition 1
^c And primitive
^d Or primitive
^e Implication
^f Implication in terms of primitives
^g Implication in terms of Haskell's Bool ordering
^h Proposition 2
^i Proposition 3
^j Proposition 4

Propositions 2 and 4 are both tautologies because they are always true.

-}

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')


type Assoc k v = [(k, v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k' == k]

type Subst = Assoc Char Bool


eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q  -- Could also do,  not eval s p || eval s q


vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q


-- My implementation of bools, which uses ideas related to the Bit type from ch7:
data Bit = Zero | One

bools :: Int -> [[Bool]]
bools n = map (map bit2Bool . padZero n . int2Bin) [0.. 2^n - 1]

int2Bit :: Int -> Bit
int2Bit x | x `mod` 2 == 0  = Zero
          | otherwise       = One

int2Bin :: Int -> [Bit]
int2Bin 0 = []
int2Bin x = int2Bit x : int2Bin (x `div` 2)

bit2Bool :: Bit -> Bool
bit2Bool Zero = False
bit2Bool One  = True

padZero :: Int -> [Bit] -> [Bit]
padZero n bs = reverse $ take n $ bs ++ (repeat Zero)


-- The book's first implementation of bools, which is basically the same as
-- mine, except with all of the details stuffed in the where clause:
bools2 :: Int -> [[Bool]]
bools2 n = map (reverse . map conv . make n . int2bin) range
  where range = [0..(2^n)-1]
        int2bin 0 = []
        int2bin x = x `mod` 2 : int2bin (x `div` 2)
        make n bs = take n (bs ++ repeat 0)
        conv 0 = False
        conv 1 = True


-- My second implementation of bools, based the book's hint about recursion:
bools3 :: Int -> [[Bool]]
bools3 0 = [[]]
bools3 n = [(b:bs) | b <- [False, True], bs <- bools3 (n-1)]


-- The book's second, recursive implementation of bools:
bools4 :: Int -> [[Bool]]
bools4 0 = [[]]
bools4 n = map (False:) bss ++ map (True:) bss
  where bss = bools4 (n-1)


rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)


-- Mine:
substs :: Prop -> [Subst]
substs p = [zip vs bs | bs <- bss]
  where vs = rmdups (vars p)
        bss = bools (length vs)

-- Book's:
substs2 :: Prop -> [Subst]
substs2 p = map (zip vs) (bools (length vs))
  where vs = rmdups (vars p)


-- Mine:
isTaut :: Prop -> Bool
isTaut p = and (map (\s -> eval s p) (substs p))

-- Book's:
isTaut2 :: Prop -> Bool
isTaut2 p = and [eval s p | s <- substs p]



tests = [
       eval [] (Const True),
  not (eval [] (Const False)),

       eval [('A', True),  ('B', True)]  (Var 'A'),
       eval [('A', True),  ('B', True)]  (Var 'B'),
       eval [('A', True),  ('B', False)] (Var 'A'),
  not (eval [('A', True),  ('B', False)] (Var 'B')),
  not (eval [('A', False), ('B', True)]  (Var 'A')),
       eval [('A', False), ('B', True)]  (Var 'B'),
  not (eval [('A', False), ('B', False)] (Var 'A')),
  not (eval [('A', False), ('B', False)] (Var 'B')),

  not (eval [] ((Not (Const True)))),
       eval [] ((Not (Const False))),

       eval [] (And (Const True)  (Const True)),
  not (eval [] (And (Const True)  (Const False))),
  not (eval [] (And (Const False) (Const True))),
  not (eval [] (And (Const False) (Const False))),

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
  bools2 3 == [[False, False, False],
               [False, False, True ],
               [False, True,  False],
               [False, True,  True ],
               [True , False, False],
               [True , False, True ],
               [True , True,  False],
               [True , True,  True ]],
  bools3 3 == [[False, False, False],
               [False, False, True ],
               [False, True,  False],
               [False, True,  True ],
               [True , False, False],
               [True , False, True ],
               [True , True,  False],
               [True , True,  True ]],

  rmdups [1, 2, 2, 3, 4, 4, 5] == [1, 2, 3, 4, 5],

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
  substs2 p4 == [[('A', False), ('B', False)],
                 [('A', False), ('B', True )],
                 [('A', True ), ('B', False)],
                 [('A', True ), ('B', True )]],

  not (isTaut p1),
       isTaut p2,
  not (isTaut p3),
       isTaut p4,
       isTaut2 p4
  ]

main :: IO ()
main = assert (and tests) putStrLn "OK"
