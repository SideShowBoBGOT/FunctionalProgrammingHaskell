module Main (main) where

import Database.PostgreSQL.Simple
  
import Lib

main :: IO ()
main = putStrLn "What is your name"