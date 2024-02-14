module Lib
    (
      oneTwo,
      twoTwo
    ) where

import Data.List
import Data.Ord

oneTwo :: (Eq a) => [a] -> [(a, Int)]
oneTwo [x] = [(x, 1)]
oneTwo l = inner (head l) 1 (head $ tail l) (tail $ tail l)
  where
    inner :: (Eq a) => a -> Int -> a -> [a] -> [(a, Int)]
    inner element count current []
      | element == current = [(current, count + 1)]
      | otherwise = [(element, count), (current, 1)]
    inner element count current other
      | element == current = inner current (count + 1) (head other) (tail other)
      | otherwise = (element, count) : inner current 1 (head other) (tail other)


twoTwo :: [a] -> Int -> [a]
twoTwo l n = [e | (_, e) <- sortBy (comparing fst) indexValues]
  where
    len = length l
    normalN = mod n len
    boundIndex index
      | index + normalN >= len = index + normalN - len
      | index + normalN < 0 = index + normalN + len
      | otherwise = index + normalN
    indexValues = [(boundIndex index, e) | (index, e) <- zip [0..] l]
