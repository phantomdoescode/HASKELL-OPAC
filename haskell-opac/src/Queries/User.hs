{-# LANGUAGE OverloadedStrings #-}

module Queries.User
  ( insertUser
  , getUserByEmail
  , getUserByID
  , getAllUsers
  , deleteUser
  ) where

import Data.Text (Text)
import Control.Monad.Reader (asks, liftIO)
import Database.SQLite.Simple (query, query_, execute)
import Database.SQLite.Simple (Only(..))
import App.Env (AppM, envConnection)
import Models.User (User(..))

insertUser :: User -> AppM ()
insertUser u = do
  conn <- asks envConnection
  liftIO $ execute conn
    "INSERT INTO users \
    \ (password_hash, first_name, last_name, email, birth_date, occupation, organization) \
    \ VALUES (?, ?, ?, ?, ?, ?, ?)"
    u

getUserByEmail :: Text -> AppM (Maybe User)
getUserByEmail email = do
  conn <- asks envConnection
  rows <- liftIO $ query conn
    "SELECT * FROM users WHERE email = ?" (Only email)
  case rows of
    []    -> return Nothing
    u : _ -> return (Just u)

getUserByID :: Int -> AppM (Maybe User)
getUserByID uid = do
  conn <- asks envConnection
  rows <- liftIO $ query conn
    "SELECT * FROM users WHERE user_id = ?" (Only uid)
  case rows of
    []    -> return Nothing
    u : _ -> return (Just u)

getAllUsers :: AppM [User]
getAllUsers = do
  conn <- asks envConnection
  liftIO $ query_ conn "SELECT * FROM users"

deleteUser :: Int -> AppM ()
deleteUser uid = do
  conn <- asks envConnection
  liftIO $ execute conn
    "DELETE FROM users WHERE user_id = ?" (Only uid)