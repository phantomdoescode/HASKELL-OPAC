{-# LANGUAGE OverloadedStrings #-}

module Queries.BorrowRecord
  ( insertBorrowRecord,
    getBorrowByID,
    getActiveBorrowsByUser,
    getAllBorrowsByUser,
    getActiveBorrowsByCopy,
    markAsReturned,
  ) where

import App.Env (AppM, envConnection)
import Control.Monad.Reader (asks, liftIO)
import Data.Time (Day)
import Database.SQLite.Simple (Only (..), execute, query)
import Models.BorrowRecord (BorrowRecord (..))

insertBorrowRecord :: BorrowRecord -> AppM ()
insertBorrowRecord r = do
  conn <- asks envConnection
  liftIO $
    execute
      conn
      "INSERT INTO borrow_records \
      \ (user_id, copy_id, borrow_date, due_date, return_date, is_returned) \
      \ VALUES (?, ?, ?, ?, ?, ?)"
      r

getBorrowByID :: Int -> AppM (Maybe BorrowRecord)
getBorrowByID bid = do
  conn <- asks envConnection
  rows <-
    liftIO $
      query
        conn
        "SELECT * FROM borrow_records WHERE borrow_id = ?"
        (Only bid)
  case rows of
    [] -> return Nothing
    r : _ -> return (Just r)

getActiveBorrowsByUser :: Int -> AppM [BorrowRecord]
getActiveBorrowsByUser uid = do
  conn <- asks envConnection
  liftIO $
    query
      conn
      "SELECT * FROM borrow_records WHERE user_id = ? AND is_returned = 0"
      (Only uid)

getAllBorrowsByUser :: Int -> AppM [BorrowRecord]
getAllBorrowsByUser uid = do
  conn <- asks envConnection
  liftIO $
    query
      conn
      "SELECT * FROM borrow_records WHERE user_id = ?"
      (Only uid)

getActiveBorrowsByCopy :: Int -> AppM [BorrowRecord]
getActiveBorrowsByCopy cid = do
  conn <- asks envConnection
  liftIO $
    query
      conn
      "SELECT * FROM borrow_records WHERE copy_id = ? AND is_returned = 0"
      (Only cid)

markAsReturned :: Int -> Day -> AppM ()
markAsReturned bid returnDate = do
  conn <- asks envConnection
  liftIO $
    execute
      conn
      "UPDATE borrow_records SET is_returned = 1, return_date = ? WHERE borrow_id = ?"
      (returnDate, bid)