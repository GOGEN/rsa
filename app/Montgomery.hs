module Montgomery
  ( montgCurve
  , add
  , double
  , multiply
  , Point(P)
  , Curve(C)
  ) where

  data Curve = C !Integer !Integer
  data Point = P !Integer !Integer

  montgCurve :: Integer -> Integer -> (Curve, Point)
  montgCurve n s = (C an ad4, P x z)
    where
      u = (s*s-5) `mod` n
      v = (4*s) `mod` n
      d = v-u
      x = (u*u*u) `mod` n
      z = (v*v*v) `mod` n
      an = ((d*d)*(d*(3*u+v))) `mod` n
      ad4 = (16*x*v) `mod` n

  add :: Integer -> Point -> Point -> Point -> Point
  add n (P x0 z0) (P x1 z1) (P x2 z2) = P x3 z3
    where
      a = (x1-z1)*(x2+z2)
      b = (x1+z1)*(x2-z2)
      c = a+b
      d = a-b
      x3 = (c*c*z0) `rem` n
      z3 = (d*d*x0) `rem` n

  double :: Integer -> Curve -> Point -> Point
  double n (C an ad4) (P x z) = P x' z'
    where
      r = x+z
      s = x-z
      u = r*r
      v = s*s
      t = u-v
      x' = (ad4*u*v) `rem` n
      z' = ((ad4*v+t*an)*t) `rem` n

  multiply :: Integer -> Point -> Curve -> Integer -> Point
  multiply k p cve n = fst $ go k
    where
      go 0 = (P 0 0, P 0 0)
      go 1 = (p, double n cve p)
      go m =  let (q, r) = m `quotRem` 2
                  (s, l) = go q
              in case r of
                0 -> (double n cve s, add n p s l)
                _ -> (add n p s l, double n cve l)
