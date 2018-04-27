module Main where

import Control.Exception (assert)



-- New types may be defined in terms of existing types:
type Pos = (Int, Int)

type Trans = Pos -> Pos

-- New types may also be defined using parameterizations:
type Pair a = (a, a)

type Assoc k v = [(k, v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k' == k]



-- The entirely new type A comprises the new values B and C (the constructors)
data A = B | C

data Move = North | South | East | West

move :: Move -> Pos -> Pos
move North (x, y) = (x, y+1)
move South (x, y) = (x, y-1)
move East  (x, y) = (x+1, y)
move West  (x, y) = (x-1, y)

moves :: [Move] -> Pos -> Pos
moves ms p = foldr move p ms

moves' :: [Move] -> Pos -> Pos
moves' []     p = p
moves' (m:ms) p = moves' ms (move m p)

rev :: Move -> Move
rev North = South
rev South = North
rev East  = West
rev West  = East



data Shape = Circle Float | Rect Float Float

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y



safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)



newtype Nat' = N Int
-- Possible alternatives:
--   type Nat = Int  -- Merely creates a synonym, Int and Nat are interchangeable
--   data Nat = N Int  -- Effectively the same, but the compiler handles newtype more efficiently

data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

add :: Nat -> Nat -> Nat
add m n = int2nat (nat2int m + nat2int n)

add' :: Nat -> Nat -> Nat
add' Zero     n = n
add' (Succ m) n = Succ (add' m n)



data List a = Nil | Cons a (List a)

len :: List a -> Int
len Nil         = 0
len (Cons _ xs) = 1 + len xs



data Tree a = Leaf a | Node (Tree a) a (Tree a)

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y)     = x == y
occurs x (Node l y r) = x == y || occurs x l || occurs x r

flatten :: Tree a -> [a]
flatten (Leaf x)     = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

-- More efficient for search trees (i.e., a tree that flattens into a sorted list)
occurs' :: Ord a => a -> Tree a -> Bool
occurs' x (Leaf y)                  = x == y
occurs' x (Node l y r) | x == y     = True
                       | x < y      = occurs' x l
                       | otherwise  = occurs' x r

-- Tree with data only in the leaves
data Tree2 a = Leaf2 a | Node2 (Tree2 a) (Tree2 a)

-- Tree with data only in the nodes
data Tree3 a = Leaf3 | Node3 (Tree3 a) a (Tree3 a)

-- Tree with different types of data in leaves and nodes
data Tree4 a b = Leaf4 a | Node4 (Tree4 a b) b (Tree4 a b)

-- Tree with a list of subtrees
data Tree5 a = Node5 a [Tree5 a]



-- Define a new type
data Bool' = True' | False'

-- Define a new type class modeled on Eq, with a default definition for /=
-- (using . in the operators to distinguish from Eq)
class Eq' a where
  (==.), (/=.) :: a -> a -> Bool

  x /=. y = not (x ==. y)

-- Define an instance of Eq for Bool; must implement the == operator
instance Eq' Bool' where
  False' ==. False' = True
  True'  ==. True'  = True
  _      ==. _      = False

-- Define another typeclass based on Eq
class Eq' a => Ord' a where
  (<.), (<=.), (>.), (>=.) :: a -> a -> Bool
  min, max :: a -> a -> a

  min x y | x <=. y   = x
          | otherwise = y
  max x y | x <=. y   = y
          | otherwise = x

-- Define an instance of Ord for Bool
instance Ord' Bool' where
  False' <. True' = True
  _      <. _     = False

  x <=. y = (x <. y) || (x ==. y)
  x >. y = y <. x
  x >=. y = y <=. x


data Bool'' = True'' | False''
  deriving (Eq, Ord, Show, Read)


tests = [
  find 'b' [('a', 1), ('b', 2), ('b', 3), ('c', 4)] == 2,

  move North (0, 0) == (0, 1),
  move South (0, 0) == (0, -1),
  move East (0, 0) == (1, 0),
  move West (0, 0) == (-1, 0),
  moves [North, East, North] (0, 0) == (1, 2),
  moves' [North, East, North] (0, 0) == (1, 2),
  move (rev North) (0, 0) == (0, -1),
  move (rev South) (0, 0) == (0, 1),
  move (rev East) (0, 0) == (-1, 0),
  move (rev West) (0, 0) == (1, 0),

  safediv 10 5 == Just 2,
  safediv 10 0 == Nothing,
  safehead [4, 5, 6] == Just 4,
  safehead ([] :: [Int]) == Nothing,  -- The type of the result is ambiguous (Maybe a) without an explicit annotation

  nat2int (Succ (Succ (Succ Zero))) == 3,
  nat2int (int2nat 3) == 3,
  nat2int (add (Succ (Succ (Succ Zero))) (Succ (Succ Zero))) == 5,
  nat2int (add' (Succ (Succ (Succ Zero))) (Succ (Succ Zero))) == 5,

  len (Cons 3 (Cons 2 (Cons 1 Nil))) == 3,

  occurs 1 t == True,
  occurs 2 t == False,
  flatten t == [1, 3, 4, 5, 6, 7, 9],
  occurs' 1 t == True,
  occurs' 2 t == False,

  True'  ==. True',
  False' ==. False',
  not (True' ==. False'),
  False' <. True',
  False' <=. True',
  True' >. False',
  True' >=. False'
  ]

main :: IO ()
main = assert (and tests) putStrLn "OK"
