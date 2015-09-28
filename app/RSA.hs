module RSA
  ( genParams
  , encrypt
  , decrypt
  ) where

  import FastDegree
  import GeneratePrimeNumber
  import Euclid
  import System.Random
  import Data.List
  import Data.Char

  type PublicKey = (Integer, Integer)
  type PrivateKey = (Integer, Integer)

  genParams :: Integer -> IO (PublicKey, PrivateKey)
  genParams bitCount = do
    g <- getStdGen
    let primeBitCount = bitCount `div` 2 - 1
    let (p, g') = genPrime g primeBitCount
    let (q, g'') = genPrime g' primeBitCount
    let n = p * q
    let phiN = (p - 1) * (q - 1)
    let e = genE g'' (bitCount `div` 3) p q phiN
    let (x, d) = euc phiN e
    return (if d > 0
            then ((n, e), (n, d))
            else ((n, e), (n, d + phiN)))


  genE :: RandomGen g => g -> Integer -> Integer -> Integer -> Integer -> Integer
  genE g bitCount p q phiN =
    let (e, g') = genPrime g bitCount
    in if e == q || e == p || phiN `mod` e == 0
      then genE g' bitCount p q phiN
      else e

  encrypt :: PublicKey -> String -> [Integer]
  encrypt (n, e) text =
    let codes = reverse $ foldl' (\xs x ->
                          let x' = show $ ord x
                          in case length x' of
                            1 -> ("00" ++ x'):xs
                            2 -> ("0" ++ x'):xs
                            3 -> x':xs
                       ) [] text
        codes' = reverse $ foldl' (\(x:xs) y -> if (read (x ++ y) :: Integer) < n
                                                then (x ++ y):xs
                                                else y:x:xs) [head codes] (drop 1 codes)
    in map (\x -> deg n (read x :: Integer) e) codes'

  decrypt :: PrivateKey -> [Integer] -> String
  decrypt (n, d) codes = foldl' (\xs x -> xs ++ decode x ) "" (map (\x -> deg n x d) codes)

  decode :: Integer -> String
  decode = decode' ""

  decode' :: String -> Integer -> String
  decode' xs 0 = xs
  decode' xs x = decode' ((chr . fromInteger . mod x $ 1000):xs) $ x `div` 1000
