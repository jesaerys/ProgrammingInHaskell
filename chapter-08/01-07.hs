module Main where


import Control.Exception (assert)



-- 1)
data Nat = Zero | Succ Nat
  deriving (Eq, Ord, Show)

nat2Int :: Nat -> Int
nat2Int Zero     = 0
nat2Int (Succ n) = 1 + nat2Int n

add :: Nat -> Nat -> Nat
add Zero     n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero     n = Zero
mult (Succ m) n = add n (mult m n)



-- 2)
-- Assume 'Tree a' is a search tree (a tree that flattens into an sorted list)
data Tree a = Leaf a | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y)     = x == y
occurs x (Node l y r) = case compare x y of
                          LT -> occurs x l
                          EQ -> True
                          GT -> occurs x r

{- In the original version, 'x' and 'y' were compared up to two times in the
guards. In this version, 'x' and 'y' are compared only once.

However, I don't understand why the new verison is any more efficient. The
branching seems simply to be deferred to the case statement, where the decision
is based on the `Ordering` value rather than direct comparisons of 'x' and 'y'.
-}



-- 3)
-- A node is balanced if the numbers of leaves on the left and right subtrees
-- differ by at most 1. A tree is balanced if all of its nodes are balanced.
data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a)
  deriving (Eq, Show)

numberOfLeaves :: Tree' a -> Int
numberOfLeaves (Leaf' _)   = 1
numberOfLeaves (Node' l r) = numberOfLeaves l + numberOfLeaves r

balanced :: Tree' a -> Bool
balanced (Leaf' _)   = True
balanced (Node' l r) = abs (numberOfLeaves l - numberOfLeaves r) <= 1
                       && balanced l
                       && balanced r



-- 4)
-- Assume [a] is nonempty
halve :: [a] -> ([a], [a])
halve xs = splitAt ((length xs) `div` 2) xs

balance :: [a] -> Tree' a
balance [x]    = Leaf' x
balance xs     = Node' (balance l) (balance r)
  where (l, r) = halve xs



-- 5)
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val x)   = f x
folde f g (Add p q) = g (folde f g p) (folde f g q)



-- 6)
eval :: Expr -> Int
eval = folde id (+)
-- The result of 'folde' must be 'Int', therefore the type var 'a' must be
-- 'Int', therefore 'id' is sufficient as the first function. I'm assuming that
-- '+' is a suitable second function because it corresponds to the name of the
-- constructor 'Add'.

size :: Expr -> Int
size (Val _)   = 1
size (Add p q) = size p + size q



-- 7)
-- The solutions here are somewhat complicated by the fact that I'm trying
-- avoid clashing with the existing 'Maybe a' and '[a]' types.
data Maybe' a = Nothing' | Just' a
  deriving Show

instance Eq a => Eq (Maybe' a) where
  Nothing' == Nothing' = True
  Just' x  == Just' y  = x == y


data List' a = Null | Cons a (List' a)
  deriving Show

instance Eq a => Eq (List' a) where
  xs' == ys' = length xs == length ys
               && and [x == y | (x, y) <- zip xs ys]
    where xs = fromList' xs'
          ys = fromList' ys'

fromList' :: List' a -> [a]
fromList' Null       = []
fromList' (Cons x list) = x : fromList' list

toList' :: [a] -> List' a
toList' [] = Null
toList' (x:xs) = Cons x (toList' xs)



tests = [
  nat2Int (Succ (Succ (Succ Zero))) == 3,
  nat2Int (add (Succ (Succ Zero)) (Succ (Succ (Succ Zero)))) == 5,
  nat2Int (mult (Succ (Succ Zero)) (Succ (Succ (Succ Zero)))) == 6,

  occurs 6 (Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))),
  not (occurs 8 (Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9)))),

  numberOfLeaves (Node' (Node' (Leaf' 1) (Leaf' 4)) (Node' (Leaf' 6) (Leaf' 9))) == 4,
  balanced (Node' (Node' (Leaf' 1) (Leaf' 4)) (Node' (Leaf' 6) (Leaf' 9))),
  balanced (Node' (Node' (Leaf' 1) (Node' (Leaf' 4) (Leaf' 5))) (Node' (Leaf' 6) (Leaf' 9))),
  not (balanced (Node' (Node' (Node' (Leaf' 1) (Leaf' 2)) (Node' (Leaf' 4) (Leaf' 5))) (Node' (Leaf' 6) (Leaf' 9)))),

  halve [1..4] == ([1, 2], [3, 4]),
  halve [1..5] == ([1, 2], [3, 4, 5]),
  balance [1, 2, 3, 4, 5, 6] == Node' (Node' (Leaf' 1) (Node' (Leaf' 2) (Leaf' 3))) (Node' (Leaf' 4) (Node' (Leaf' 5) (Leaf' 6))),
  balance [1, 2, 3, 4, 5] == Node' (Node' (Leaf' 1) (Leaf' 2)) (Node' (Leaf' 3) (Node' (Leaf' 4) (Leaf' 5))),

  folde show (++) (Add (Add (Val 1) (Val 2)) (Val 3)) == "123",

  eval (Add (Add (Val 1) (Val 2)) (Val 3)) == 6,
  size (Add (Add (Val 1) (Val 2)) (Val 3)) == 3,

  (Nothing' :: Maybe' Int) == (Nothing' :: Maybe' Int),
  Just' 4  == Just' 4,
  not (Just' 4  == Just' 5),

  (toList' ([] :: [Int])) == (toList' ([] :: [Int])),
  (toList' [1, 2, 3]) == (toList' [1, 2, 3]),
  not ((toList' [1, 2, 3]) == (toList' [1, 2])),

  True
  ]

main :: IO ()
main = assert (and tests) putStrLn "OK"
