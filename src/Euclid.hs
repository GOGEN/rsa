module Euclid
  ( euc
  ) where
    
  euc :: (Integral a) => a -> a -> (a, a)
  euc a b = case d of
              0 -> (0, 1)
              _ -> let (e, f) = euc b d
                   in (f, e - c*f)
    where c = a `div` b
          d = a `mod` b
