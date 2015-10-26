{-# LANGUAGE MonadComprehensions, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module HW07 where

import Prelude hiding (mapM)
import Cards

import Control.Monad hiding (mapM, liftM)
import Control.Monad.Random
import Data.Functor
import Data.Monoid
import Data.Vector (Vector, cons, (!), (!?), (//))
import System.Random

import qualified Data.Vector as V


-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f ma = ma >>= \x -> return (f x)
liftM' f ma = do
  x <- ma
  return (f x)

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV i j v = do
  vi <- v !? i
  vj <- v !? j
  return $ v // [(i, vj), (j, vi)]

swapV' i j v = liftM2 (\vi vj -> v // [(i, vj), (j, vi)]) (v !? i) (v !? j)

-- Exercise 2 -----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM _ [] = return []
mapM f (a:as) = liftM2 (:) (f a) (mapM f as)

getElts :: [Int] -> Vector a -> Maybe [a]
getElts idxs v = mapM (v !?) idxs

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt v = (v !?) <$> getRandomR (0, V.length v)

-- Exercise 4 -----------------------------------------

randomVec :: Random a => Int -> Rnd (Vector a)
randomVec n = V.fromList <$> replicateM n getRandom

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR n x = V.fromList <$> replicateM n (getRandomR x)

-- Exercise 5 -----------------------------------------

shuffle :: Vector a -> Rnd (Vector a)
shuffle v = foldr swap v <$> mapM getRandomX [1..n]
  where
    getRandomX i = (\j -> (i, j)) <$> getRandomR (0, i)
    n = V.length v
    swap (x, y) v' = v' // [(x, v' ! y), (y, v' ! x)]

-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt v idx = ((V.filter (<value) v), value, (V.filter (>=value) v))
                    where value = v ! idx

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

qsort :: Ord a => Vector a -> Vector a
qsort v
  | V.null v = V.empty
  | otherwise = qsort [ y | y <- xs, y < x ]
                <> (cons x $ qsort [ y | y <- xs, y >= x ])
  where
    x = V.head v
    xs = V.tail v


-- Exercise 8 -----------------------------------------

qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR v
  | V.null v = return V.empty
  | otherwise = do
    idx <- getRandomR (0, V.length v - 1)
    let (first, x, second) = partitionAt v idx
    let v' = (first <> second)
    first' <- qsortR [ y | y <- v', y < x]
    second' <- qsortR [ y | y <- v', y >= x]
    return $ first' <> (cons x second')
-- Exercise 9 -----------------------------------------

-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select n v
  | n >= V.length v = return Nothing
  | otherwise = do
      idx <- getRandomR (0, V.length v - 1)
      let (first, x, second) = partitionAt v idx
      let v' = (first <> second)
      let first' = [y | y <- v', y < x]
      let second' = [y | y <- v', y >= x]
      if (V.length first') < n
        then select (n - (V.length first') - 1) second'
        else if (V.length first') == n
          then return $ Just x
          else select n first'

-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = [ Card label suit | suit <- suits, label <- labels ]

newDeck :: Rnd Deck
newDeck =  shuffle allCards

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard deck
  | V.null deck = Nothing
  | otherwise = Just (V.head deck, V.tail deck)

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards n deck = getCards' n [] deck
  where
    getCards' n ret deck
      | n == 0 = Just (ret, deck)
      | otherwise = do
        (card, deck') <- nextCard(deck)
        getCards' (n - 1) (ret ++ [card]) deck'

-- Exercise 13 ----------------------------------------

data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else do
              case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:\n" ++ show c1
                  putStrLn $ "I got:\n" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100
