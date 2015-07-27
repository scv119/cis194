{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map
imoprt qualified Data.Bits as Bits

import Parser

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret originalFile encryptedFile = do
  originalContent <- BS.readFile originalFile
  encryptedContent <- Bs.readFile encryptedFile
  return filter (\word -> word /= (fromIntegral 0))) $ map (\(word0, word1) -> Bits.xor word0 word) $ zip originalContent encryptedContent

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key encryptedFile = do
  encryptedContent <- BS.readFile $ encryptedFile ++ ".enc"
  let decypher content = map (\(word0, word1) -> Bits.xor word0 word1) $ zip content $ cycle key
  BS.writeFile $ decypher encryptedFile encryptedFile

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile jsonFilePath = do
  fileContent <- BS.readFile jsonFilePath
  return decode fileContent

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victimsListFile transactionFile = do
  maybeVictims <- parseFile victimsListFile :: IO (Maybe [TId])
  maybeTransactions <- parseFile transactionFile :: IO (Maybe [Transaction])
  return getBadTs' maybeVictims maybeTransactions
  where
    getBadTs'
      | Nothing _ = Nothing
      | (Just victims) maybeTransactions = map (\t -> any (\victim -> victim == tid t) victims) <$> maybeTransactions

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow = foldl addTransaction emty
  where
    addTransaction' flowMap name value = let balance = lookup name flowMap
      in case balance of
        Nothing -> insert name value flowMap
        Just b -> insert name (value + b) flowMap
    addTransaction flowMap transaction =
      addTransaction flowMap (from transaction) ((-1) * (amount transaction))
      addTransaction flowMap (to transaction) (amount transaction)
-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal = undefined

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs = undefined

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON = undefined

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <-
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim
