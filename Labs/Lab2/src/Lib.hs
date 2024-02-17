module Lib
    ( oneOneA,
      oneOneB,
      oneOneC,
      twoOneA,
      twoOneB,
      someFunc
    ) where

import Data.List
  
someFunc :: IO ()
someFunc = putStrLn "someFunc"

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ [] = []
myTakeWhile p (x:xs)
  | p x = x : myTakeWhile p xs
  | otherwise = []
  
myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile _ [] = []
myDropWhile p xs@(x:xs')
    | p x = myDropWhile p xs'
    | otherwise = xs 

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

oneOneA :: Eq a => [a] -> [(a, Int)]
oneOneA [] = []
oneOneA (x:xs) = (x, myLength (myTakeWhile (== x) (x:xs))) : oneOneA (myDropWhile (== x) xs)

oneOneB :: Eq a => [a] -> [(a, Int)]
oneOneB [] = []
oneOneB (x:xs) = (x, length (takeWhile (== x) (x:xs))) : oneOneB (dropWhile (== x) xs)

oneOneC :: Eq a => [a] -> [(a, Int)]
oneOneC l = map (\xs@(x:_) -> (x, length xs)) $ group l 

myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake n (x:xs) = x : myTake (n - 1) xs

myDrop :: Int -> [a] -> [a]
myDrop 0 l = l
myDrop n (_:xs) = myDrop (n - 1) xs

myMod :: Int -> Int -> Int
myMod _ 0 = error "Division by zero is undefined"
myMod x y
    | x < 0 = myMod (x + y) y
    | x < y = x
    | otherwise = myMod (x - y) y

twoOneA :: [a] -> Int -> [a]
twoOneA l n = myDropK l ++ myTakeK l
  where
    k = myMod n (myLength l)
    myDropK = myDrop k
    myTakeK = myTake k

twoOneB :: [a] -> Int -> [a]
twoOneB l n = dropK l ++ takeK l
  where
    k = mod n (myLength l)
    dropK = drop k
    takeK = take k