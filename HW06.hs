{-# OPTIONS_GHC -Wall #-}
module HW06 where

import Data.List
import Data.Functor

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib n
  | n < 2 = 1
  | otherwise = (fib (n - 1)) + (fib (n - 2))

fibs1 :: [Integer]
fibs1 = map (\x -> fib x) [0..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = [1] ++ fibs2'
  where fibs2' = zipWith (+) (0:1:fibs2') (1:fibs2')

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons a b) = a:(streamToList b)

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap g (Cons a b) = Cons (g a) (fmap g b)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat a = Cons a (sRepeat a)

sIterate :: (a -> a) -> a -> Stream a
sIterate unfold seed = Cons seed (sIterate unfold (unfold seed))

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons first remain) second = Cons first (sInterleave second remain)

sTake :: Int -> Stream a -> [a]
sTake n (Cons first remain)
  | n == 0 = []
  | otherwise = first:(sTake (n - 1) remain)

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate (+1) 0

ruler :: Stream Integer
ruler = foldr1 sInterleave $ map sRepeat $ streamToList nats

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand x = Cons x $ rand ((1103515245 * x + 12345) `mod` 2147483648)

-- Exercise 8 -----------------------------------------

{- Total Memory in use: ??? MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: ??? MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing
minMax [x] = Just (x, x)
minMax (x:xs) = minMax xs >>= (\(minV, maxV) -> Just ((min minV x), (max maxV x)))


main :: IO ()
main = print $ minMaxSlow $ sTake 500000 $ rand 7666532

-- Exercise 10 ----------------------------------------

fastFib :: Int -> Integer
fastFib = undefined
