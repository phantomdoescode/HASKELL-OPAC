{-# LANGUAGE OverloadedStrings #-}

module Queries.AuditLog
  ( insertAuditLog
  , getAllAuditLogs
  ) where

import App.Env (AppM, envConnection)
import Control.Monad.Reader (asks, liftIO)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Database.SQLite.Simple (execute, query_)

insertAuditLog :: Maybe Int -> Text -> AppM ()
insertAuditLog mUid action = do
  conn <- asks envConnection
  now <- liftIO getCurrentTime
  liftIO $ execute conn
    "INSERT INTO audit_logs (user_id, action, timestamp) VALUES (?, ?, ?)"
    (mUid, action, show now)

getAllAuditLogs :: AppM [(Int, Maybe Int, Text, Text)]
getAllAuditLogs = do
  conn <- asks envConnection
  liftIO $ query_ conn "SELECT * FROM audit_logs ORDER BY timestamp DESC"
