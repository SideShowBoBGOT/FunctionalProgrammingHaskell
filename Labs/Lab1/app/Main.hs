module Main (main) where

import Lib

main :: IO ()
main = someFunc (1, 1, 1)

someFunc x y z = print x
