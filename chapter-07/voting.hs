module Voting where

import Control.Exception (assert)
import Data.List


votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

rmdups' :: Eq a => [a] -> [a]
rmdups' [] = []
rmdups' (x:xs) = x : filter (/= x) (rmdups' xs)

result :: Ord a => [a] -> [(Int, a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]

winner :: Ord a => [a] -> a
winner = snd . last . result


ballots :: [[String]]
ballots = [
    ["Red", "Green"],
    ["Blue"],
    ["Green", "Red", "Blue"],
    ["Blue", "Green", "Red"],
    ["Green"]
    ]

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
    [c] -> c
    (c:cs) -> winner' (elim c bs)


tests = [
  count "Red" votes == 2,
  rmdups votes == ["Red", "Blue", "Green"],
  rmdups' votes == ["Red", "Blue", "Green"],
  result votes == [(1, "Green"), (2, "Red"), (3, "Blue")],
  winner votes == "Blue",

  rmempty [[1, 2], [], [3, 4, 5]] == [[1, 2], [3, 4, 5]],
  elim "Red" ballots == [["Green"], ["Blue"], ["Green", "Blue"], ["Blue", "Green"], ["Green"]],
  rank ballots == ["Red", "Blue", "Green"],
  winner' ballots == "Green"
  ]

main :: IO ()
main = assert (and tests) putStrLn "OK"
