module Main where

import Lib
import Euclid
import FastDegree
import GeneratePrimeNumber
import System.Random

main :: IO ()
main = do
  print $ euc 172 38
  print $ deg 19 5 13
  g <- getStdGen
  rank <- getLine
  let (r, g') = genPrime g (read rank :: Integer)
  print r
