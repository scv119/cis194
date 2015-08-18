{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Control.Applicative
import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map
import qualified Data.Bits as Bits

import Parser

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret originalFile encryptedFile = do
  originalContent <- BS.readFile originalFile
  encryptedContent <- BS.readFile encryptedFile
  return $ BS.pack $ filter (\word -> word /= (fromIntegral 0)) $ map (\(word0, word1) -> Bits.xor word0 word1) $ BS.zip originalContent encryptedContent

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key encryptedFile = do
  encryptedContent <- BS.readFile $ encryptedFile ++ ".enc"
  let decypher content = BS.pack $ map (\(word0, word1) -> Bits.xor word0 word1) $ BS.zip content $ BS.cycle key
  BS.writeFile encryptedFile $ decypher encryptedContent

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile jsonFilePath = do
  fileContent <- BS.readFile jsonFilePath
  return $ decode fileContent

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victimsListFile transactionFile = do
  maybeVictims <- parseFile victimsListFile :: IO (Maybe [TId])
  maybeTransactions <- parseFile transactionFile :: IO (Maybe [Transaction])
  return $ getBadTs' maybeVictims maybeTransactions
  where
    getBadTs' maybeV maybeT = case maybeV of
      Nothing -> Nothing
      (Just victims) -> (filter (\t -> any (\victim -> victim == tid t) victims)) <$> maybeT

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow = foldl addTransaction Map.empty
  where
    addTransaction' flowMap name value =
      let balance = Map.lookup name flowMap
      in case balance of
        Nothing -> Map.insert name value flowMap
        Just b -> Map.insert name (value + b) flowMap
    addTransaction flowMap transaction =
      addTransaction' flowMap1 (from transaction) ((-1) * (amount transaction))
      where
        flowMap1 = addTransaction' flowMap (to transaction) (amount transaction)

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal = fst . Map.foldrWithKey findMax ("", 0)
  where
    findMax name value (currentName, maxValue) = if (value < maxValue) then (currentName, maxValue) else (name, value)

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
