module GeneratePrimeNumber
  ( genPrime
  ) where

  import System.Random
  import FastDegree

  genPrime :: RandomGen g => g -> Integer -> (Integer, g)
  genPrime g k =
    let bottomBound = 2 ^ k
        upperBound = bottomBound * 2 - 1
        (r, g') = randomR (bottomBound, upperBound) g
        r' = if r `mod` 2 == 0 then r + 1 else r
    in genPrime' g' k r'

  genPrime' :: RandomGen g => g -> Integer -> Integer -> (Integer, g)
  genPrime' g k r
    | r >= 10 ^ (k + 1) = genPrime g k
    | otherwise = case testML r 5 g of
                    (True, g') -> (r, g')
                    (_, g') -> genPrime' g' k (r+2)



  testML :: RandomGen g => Integer -> Integer -> g -> (Bool, g)
  testML _ 0 g = (True, g)
  testML r k g =
    let (a, g') = randomR (2, r-2) g
        (s, t) = decomposition (r - 1)
    in case r `mod` a of
        0 -> (False, g')
        _ -> case deg r a t of
              1 -> testML r (k - 1) g'
              b -> (testB b r s, g')

  testB :: Integer -> Integer -> Integer -> Bool
  testB b r s = testB' (deg r b 2) r s 1

  testB' b r s i
    | i == s = False
    | b == r - 1 = True
    | otherwise = testB' (deg r b 2) r s (i + 1)

  decomposition :: Integral a => a -> (a, a)
  decomposition x = decomposition' x 0

  decomposition' :: (Integral a, Integral b) => a -> b -> (b, a)
  decomposition' a s = case a `mod` 2 of
                        0 -> decomposition' (a `div` 2) (s + 1)
                        _ -> (s, a)
