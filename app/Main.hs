module Main where

import Lib
import Euclid
import FastDegree

main :: IO ()
main = do
  print $ euc 172 38
  print $ deg 19 5 13
