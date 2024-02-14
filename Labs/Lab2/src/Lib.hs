module Lib
    ( oneOneA,
      someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

oneOneA :: (Eq a) => [a] -> [(a, Int)]
oneOneA [x] = [(x, 1)]
oneOneA l = inner (head l) 1 (head $ tail l) (tail $ tail l)
  where
    inner :: (Eq a) => a -> Int -> a -> [a] -> [(a, Int)]
    inner element count current []
      | element == current = [(current, count + 1)]
      | otherwise = [(element, count), (current, 1)]
    inner element count current other
      | element == current = inner current (count + 1) (head other) (tail other)
      | otherwise = (element, count) : inner current 1 (head other) (tail other)
      
oneOneB :: (Eq a) => [a] -> [(a, Int)]
oneOneB [x] = [(x, 1)]
oneOneB 

