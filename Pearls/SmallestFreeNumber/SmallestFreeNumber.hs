module SmallestFreeNumber where

import qualified Data.Set as S
import Data.Array
import Data.List

-- Problem statement: given a list of numbers, return
-- the smallest natural number not in this set.

-- My first dumb solution.

slowSmallestFree :: [Int] -> Int
slowSmallestFree l = head [ x | x <- [0..], not (x `elem` l)]

-- A better one with the right data structure: a set to lookup
-- an element efficiently.

setSmallestFree :: [Int] -> Int
setSmallestFree l = head [ x | x <- [0..], not (S.member x s)]
                    where s = S.fromList l

-- An even better one with set difference.

diffSmallestFree :: [Int] -> Int
diffSmallestFree l = S.findMin (S.difference allNats s)
                     where len = length l
                           s = S.fromList l
                           allNats = S.fromList [0..len]


------------------------------- Bird's solutions -------------------------------

-- O(n^2) solution, just the specification.

minFree :: [Int] -> Int
minFree xs = head (filter (\x -> not (x `elem` xs)) [0..])

-- Linear time array solution. It uses the fact that not all naturals
-- in [0..length l] can be in l, and thus can use an array with these
-- indexes to compute the smallest free number.

arrMinFree :: [Int] -> Int
arrMinFree = search . checklist

search :: Array Int Bool -> Int
search = length . takeWhile id . elems

-- Uses the fact that accumArray is linear in the association list,
-- as long as the association function is constant time.

checklist :: [Int] -> Array Int Bool
checklist l = accumArray (||)
                         False
                         (0,len)
                         (zip (filter (<= len) l) (repeat True))
              where len = length l

-- Beautiful divide and conquer solution.

divMinFree :: [Int] -> Int
divMinFree xs = minFrom 0 (length xs,xs)

minFrom :: Int -> (Int,[Int]) -> Int
minFrom a (n,xs)
  | n == 0 = a
  | m == b - a = minFrom b (n-m,vs)
  | otherwise = minFrom a (m, us)
  where (us,vs) = partition (< b) xs
        b = a + 1 + n `div` 2
        m = length us

