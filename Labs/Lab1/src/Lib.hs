module Lib
    (
      oneOne
    ) where

oneOne :: (Eq a) => [a] -> [a]
oneOne [] = []
oneOne [x] = [x]
oneOne (x:xs) = x : inner x (head xs) (tail xs)
  where
    inner :: (Eq a) => a -> a -> [a] -> [a]
    inner current headElement []
      | current == headElement = []
      | otherwise = [headElement]
    inner current headElement (x':xs')
      | current == headElement = inner headElement x' xs'
      | otherwise = headElement : inner headElement x' xs'

--isInside :: (Eq a) => a -> [a] -> Bool
--isInside _ [] = False
--isInside e (x:xs) = e == x || isInside e xs
--
--hasPath :: (Eq a) => (a, a) -> [(a, a)] -> Bool
--hasPath _ [] = False
--hasPath search (x:xs) = hasPathImpl search x [] xs
--
--hasPathImpl :: (Eq a) => (a, a) -> (a, a) -> [(a, a)] -> [(a, a)] -> Bool
--hasPathImpl search current _ [] = snd search == snd current
--hasPathImpl search current visited remainder
--  | snd search == snd current = True
--  | isInside current visited = hasPathImpl search (head remainder) visited (tail remainder)
--  | fst search == fst current = hasPathImpl (snd current, snd search) (head remainder) (current : visited) (tail remainder)
--  | otherwise = hasPathImpl search (head remainder) (current : visited) (tail remainder)