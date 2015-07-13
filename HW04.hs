{-# OPTIONS_GHC -Wall #-}
module HW04 where

import Data.List

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    P x == P y = x == y
    _ == _ = False
 
-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P []) = "0"
    show (P x) = intercalate " + " $ map showTerm $ filter (\(_, coeff) -> coeff /= 0) $ reverse $ zip [0..] x 
      where
        showTerm (degree, coeff)
          | degree == 0 = show coeff
          | coeff == 1 = "x^" ++ show degree
          | coeff == -1 = "-x^" ++ show degree
          | otherwise = show coeff ++ "x^" ++ show degree

-- Exercise 4 -----------------------------------------

plus :: (Num a) => Poly a -> Poly a -> Poly a
plus (P x) (P y) = P $ zipWith (+) x y ++ drop minLen x ++ drop minLen y where
  minLen = min (length x) (length y)

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times = undefined

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate      = undefined
    fromInteger = undefined
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP = undefined

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv = undefined

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv = undefined

