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
    (pub, priv) <- genParams (read rank :: Integer)
    print (pub, priv)
    let text = "I DON'T NEED GOOGLE MY WIFE KNOWS EVERYTHING"
    print text
    let codes = encrypt pub text
    print ("encrypt result: " ++ show codes)
    let text' = decrypt priv codes
    print ("decrypt result: " ++ text')
