{-# LANGUAGE OverloadedStrings #-}

module Queries.Copy
  ( insertCopy
  , getCopyByID
  , getCopiesByBookID
  , updateCopyStatus
  , getAvailableCopy
  ) where

import App.Env (AppM, envConnection)
import Control.Monad.Reader (asks, liftIO)
import Data.Text (Text)
import Database.SQLite.Simple (Only (..), execute, query)
import Models.Copy (Copy (..))

insertCopy :: Copy -> AppM ()
insertCopy c = do
  conn <- asks envConnection
  liftIO $ execute conn
    "INSERT INTO copies (book_id, control_number, status) VALUES (?, ?, ?)"
    (copyBookID c, copyControlNumber c, copyStatus c)

getCopyByID :: Int -> AppM (Maybe Copy)
getCopyByID cid = do
  conn <- asks envConnection
  rows <- liftIO $ query conn "SELECT * FROM copies WHERE copy_id = ?" (Only cid)
  case rows of
    [] -> return Nothing
    c : _ -> return (Just c)

getCopiesByBookID :: Int -> AppM [Copy]
getCopiesByBookID bid = do
  conn <- asks envConnection
  liftIO $ query conn "SELECT * FROM copies WHERE book_id = ?" (Only bid)

updateCopyStatus :: Int -> Text -> AppM ()
updateCopyStatus cid status = do
  conn <- asks envConnection
  liftIO $ execute conn "UPDATE copies SET status = ? WHERE copy_id = ?" (status, cid)

getAvailableCopy :: Int -> AppM (Maybe Copy)
getAvailableCopy bid = do
  conn <- asks envConnection
  rows <- liftIO $ query conn 
    "SELECT * FROM copies WHERE book_id = ? AND status = 'Available' LIMIT 1" 
    (Only bid)
  case rows of
    [] -> return Nothing
    c : _ -> return (Just c)
