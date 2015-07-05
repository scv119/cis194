{-# OPTIONS_GHC -Wall #-}
module HW02 where

import Data.List

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches actual guess = foldl (\x [y, z] -> if y == z then (1 + x) else x) 0 transposed
  where transposed = transpose [actual, guess]

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys

countColors :: Code -> [Int]
countColors code = map (\color -> countColor color code) colors
  where countColor color code = foldl (\x y -> if color == y then (1 + x) else x) 0 code

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches guess actual = foldl (\x ([y, z]) -> x + min y z) 0 transposed
  where transposed = transpose [countColors guess, countColors actual]

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess = Move guess exactMatch nonExactMatch
  where
    exactMatch = exactMatches secret guess
    nonExactMatch = matches secret guess - exactMatch

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move code e n) code1 = (getMove code code1) == (Move code1 e n)

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes m x = filter (\code1 -> isConsistent m code1) x
-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes n
  | n == 0 = [[]]
  | otherwise = expand $ allCodes $ n - 1
    where expand codes = concat $ map (\c -> (map (\cc -> cc:c) colors)) codes

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve = undefined

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
