{-# LANGUAGE OverloadedStrings #-}

module Queries.User
  ( insertUser
  , getUserByEmail
  , getUserByID
  , getAllUsers
  , updateUserProfile
  , updateUserPassword
  , updateUserFineBalance
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
    \ (password_hash, first_name, last_name, email, birth_date, occupation, organization, fine_balance, user_type) \
    \ VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
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

updateUserProfile :: User -> AppM ()
updateUserProfile u = do
  conn <- asks envConnection
  liftIO $ execute conn
    "UPDATE users SET first_name = ?, last_name = ?, occupation = ?, organization = ? WHERE user_id = ?"
    ( userFirstName u
    , userLastName u
    , userOccupation u
    , userOrganization u
    , userID u
    )

updateUserPassword :: Int -> Text -> AppM ()
updateUserPassword uid hash = do
  conn <- asks envConnection
  liftIO $ execute conn
    "UPDATE users SET password_hash = ? WHERE user_id = ?"
    (hash, uid)

updateUserFineBalance :: Int -> Double -> AppM ()
updateUserFineBalance uid fine = do
  conn <- asks envConnection
  liftIO $ execute conn
    "UPDATE users SET fine_balance = ? WHERE user_id = ?"
    (fine, uid)

deleteUser :: Int -> AppM ()
deleteUser uid = do
  conn <- asks envConnection
  liftIO $ execute conn
    "DELETE FROM users WHERE user_id = ?" (Only uid)