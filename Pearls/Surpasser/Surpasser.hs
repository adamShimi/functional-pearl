module Surpasser where

import Data.List

-- A surpasser of an element x_i in an array
-- is greater element x_j such that j > i (and x_j > x_i)
-- The problem is to find the maximum number of surpasser
-- of an element, in time O(log(n))

-- The obvious slow solution. The issue here is that
-- running over each tail requires n*(n-1)/2 = O(n^2)
-- time.

slowSurpasser :: Ord a => [a] -> Int
slowSurpasser l = maximum [ (length . filter (> (head) t) . tail) t | t <- tails l,
                                                                      t /= []]

-- Vodoo divide-and-conquer solution in O(nlog(n))

msc :: Ord a => [a] -> Int
msc = maximum . (map snd) . table

table :: Ord a => [a] -> [(a,Int)]
table [x] = [(x,0)]
table xs = joinT (m-n) (table ys) (table zs)
           where m = length xs
                 n = m `div` 2
                 (ys,zs) = splitAt n xs

joinT :: Ord a => Int -> [(a,Int)] -> [(a,Int)] -> [(a,Int)]
joinT 0 txs [] = txs
joinT n [] tys = tys
joinT n txs@((x,c):txs') tys@((y,d):tys')
  | x < y = ((x,c+n):(joinT n txs' tys))
  | otherwise = ((y,d):(joinT (n-1) txs tys'))
