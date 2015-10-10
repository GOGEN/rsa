module Main where

  import Lib
  import Euclid
  import FastDegree
  import GeneratePrimeNumber
  import RSA
  import Factorization
  import System.Random
  import CustomCodes

  main :: IO ()
  main = do
    -- g <- getStdGen
    -- print $ euc 172 38
    -- print $ deg 19 5 13
    -- rank <- getLine
    -- let (r, g') = genPrime g (read rank :: Integer)
    -- print r
    -- rank <- getLine
    -- (pub, priv) <- genParams (read rank :: Integer)
    -- print (pub, priv)
    -- let text = "I DON'T NEED GOOGLE MY WIFE KNOWS EVERYTHING"
    -- print text
    -- let codes = encrypt pub text
    -- print ("encrypt result: " ++ show codes)
    -- let text' = decrypt priv codes
    -- print ("decrypt result: " ++ text')
    let n = 342009136322315980094889308153
    let e = 208882210355060386981711608527
    let sw = 323071158829636363902958789743
    pq <- fact n
    print pq
    let pk = calcPrivateKey pq e
    let res = customDecrypt pk sw
    putStrLn res
