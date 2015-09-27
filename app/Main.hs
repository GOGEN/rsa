module Main where

import Lib
import Euclid
import FastDegree
import GeneratePrimeNumber
import RSA
import System.Random

main :: IO ()
main = do
  -- print $ euc 172 38
  -- print $ deg 19 5 13
  -- g <- getStdGen
  -- rank <- getLine
  -- let (r, g') = genPrime g (read rank :: Integer)
  -- print r
  rank <- getLine
  params <- genParams (read rank :: Integer)
  print params
