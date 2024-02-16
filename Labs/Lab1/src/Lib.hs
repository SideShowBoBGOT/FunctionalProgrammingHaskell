module Lib
    (
      oneTenA,
      oneTenB,
      twoTenA,
      twoTenB
    ) where

import Data.List

oneTenA :: [a] -> a
oneTenA [] = error "empty list"
oneTenA [_] = error "only one element"
oneTenA [x, _] = x
oneTenA (_:xs) = oneTenA xs

oneTenB :: [a] -> a
oneTenB xs = last (init xs)

myLast :: [a] -> a
myLast [] = error "empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

myInit :: [a] -> [a]
myInit [] = error "empty list"
myInit [_] = []
myInit (x:xs) = x : myInit xs

twoTenA :: [a] -> Int -> [a]
twoTenA l 0 = l
twoTenA l n = twoTenA (myLast l : myInit l) (n - 1)

twoTenB :: [a] -> Int -> [a]
twoTenB l 0 = l
twoTenB l n = twoTenA (last l : init l) (n - 1)

