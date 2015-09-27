module RSA
    ( genParams
    ) where

    import FastDegree
    import GeneratePrimeNumber
    import Euclid
    import System.Random

    type PublicKey = (Integer, Integer)
    type PrivateKey = (Integer, Integer)

    genParams :: Integer -> IO (PublicKey, PrivateKey)
    genParams bitCount = do
      g <- getStdGen
      let primeBitCount = bitCount `div` 2
      let (p, g') = genPrime g primeBitCount
      let (q, g'') = genPrime g' primeBitCount
      let n = p * q
      let phiN = (p - 1) * (q - 1)
      let e = genE g'' bitCount p q phiN
      let (_, d) = euc phiN e
      return (if d > 0 then ((n, e), (phiN, d)) else ((n, e), (phiN, d + phiN)))


    genE :: RandomGen g => g -> Integer -> Integer -> Integer -> Integer -> Integer
    genE g bitCount p q phiN =
      let (e, g') = genPrime g (bitCount `div` 3)
      in if e == q || e == p || phiN `mod` e == 0
        then genE g' bitCount p q phiN
        else e
