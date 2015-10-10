{-# LANGUAGE MagicHash #-}
{-# OPTIONS_HADDOCK hide #-}

module Factorization
  ( montgomeryFact
  , fact
  ) where

  import System.Random
  import Montgomery
  import GHC.Integer.Logarithms
  import GHC.Integer.GMP.Internals
  import Data.Numbers.Primes
  import Data.List
  import Data.Maybe

  fact :: Integer -> IO (Maybe Integer)
  fact n = do
    gen <- getStdGen
    return $ fact' gen n

  fact' :: RandomGen g => g -> Integer -> Maybe Integer
  fact' gen n =
    let params =  [ (12, 400, 10000, 10), (15, 2000, 50000, 25), (20, 11000, 150000, 90)
                  , (25, 50000, 500000, 300), (30, 250000, 1500000, 700)
                  , (35, 1000000, 4000000, 1800), (40, 3000000, 12000000, 5100)
                  , (45, 11000000, 45000000, 10600), (50, 43000000, 200000000, 19300)
                  , (55, 80000000, 400000000,30000), (60, 120000000, 700000000, 50000)
                  ]
        step gen n b_1 b_2 st_ind =
          if st_ind == 0
          then (gen, Nothing)
          else case montgomeryFact n b_1 b_2 gen of
                          (gen', Nothing) -> step gen' n b_1 b_2 (st_ind - 1)
                          res -> res
        result = Nothing
    in snd $ foldl'
              (\(gen, result) (_, b_1, b_2, count) -> if  isNothing result
                                                          then step gen n b_1 b_2 count
                                                          else (gen, result))
              (gen, result)
              params

  montgomeryFact :: RandomGen g => Integer -> Integer -> Integer -> g -> (g, Maybe Integer)
  montgomeryFact n b_1 b_2 gen =
    let (s, gen') = randomR (7, n - 1) gen
        (curve, p0) = montgCurve n s
        (_:primeList) = takeWhile (< b_2) primes
        paList = map
                  (\prime -> prime ^ bitInteger (integerLogBase# prime b_1))
                  $ takeWhile (< b_1) primeList
        q@(P q_x q_z) = foldl'
                          (\q pa -> multiply pa q curve n )
                          p0
                          paList
        g = gcd n q_z
        (g', q') = foldl'
                    (\(g, q) prime -> let q'@(P q_x q_z) = multiply prime q curve n
                                      in ((g * q_z) `mod` n , q'))
                    (g, q)
                    (dropWhile (< b_1) primeList)
        g'' = gcd g' n
    in  if 1 < g && g < n
        then (gen', Just g)
        else  if 1 < g'' && g'' < n
              then (gen', Just g'')
              else (gen', Nothing)
