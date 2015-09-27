module FastDegree
    ( deg
    ) where

    deg :: Integral a => Show a => a -> a -> a -> a
    deg _ a 0 = 1
    deg n a 1 = a `mod` n
    deg n a e = case e `mod` 2 of
              0 -> c * c `mod` n
              1 -> c * c * a `mod` n
      where c = deg n a (e `div` 2)
