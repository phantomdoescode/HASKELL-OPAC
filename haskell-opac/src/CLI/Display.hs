{-# LANGUAGE OverloadedStrings #-}

module CLI.Display
  ( printHeader
  , printSuccess
  , printError
  , printInfo
  , printSeparator
  , printBook
  , printUser
  , printBorrowRecord
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Models.Book (Book(..))
import Models.User (User(..))
import Models.BorrowRecord (BorrowRecord(..))

printSeparator :: IO ()
printSeparator = putStrLn (replicate 40 '-')

printHeader :: Text -> IO ()
printHeader title = do
  printSeparator
  TIO.putStrLn $ "  " <> title
  printSeparator

printSuccess :: Text -> IO ()
printSuccess msg = TIO.putStrLn $ "[OK] " <> msg

printError :: Text -> IO ()
printError msg = TIO.putStrLn $ "[ERROR] " <> msg

printInfo :: Text -> IO ()
printInfo msg = TIO.putStrLn $ "[INFO] " <> msg

printBook :: Book -> IO ()
printBook b = do
  printSeparator
  TIO.putStrLn $ "Title      : " <> bookTitle b
  TIO.putStrLn $ "Author     : " <> bookAuthor b
  TIO.putStrLn $ "ISBN       : " <> bookISBN b
  TIO.putStrLn $ "Genre      : " <> bookGenre b
  TIO.putStrLn $ "Publisher  : " <> bookPublication b

printUser :: User -> IO ()
printUser u = do
  printSeparator
  TIO.putStrLn $ "Name       : " <> userFirstName u <> " " <> userLastName u
  TIO.putStrLn $ "Email      : " <> userEmail u
  TIO.putStrLn $ "Occupation : " <> userOccupation u
  TIO.putStrLn $ "Org        : " <> userOrganization u
  TIO.putStrLn $ "Fines      : $" <> T.pack (show $ userFineBalance u)

printBorrowRecord :: BorrowRecord -> IO ()
printBorrowRecord r = do
  printSeparator
  putStrLn $ "Borrow ID  : " <> show (borrowID r)
  putStrLn $ "Copy ID    : " <> show (borrowCopyID r)
  putStrLn $ "Borrowed   : " <> show (borrowDate r)
  putStrLn $ "Due        : " <> show (borrowDueDate r)
  putStrLn $ "Returned   : " <> show (borrowIsReturned r)