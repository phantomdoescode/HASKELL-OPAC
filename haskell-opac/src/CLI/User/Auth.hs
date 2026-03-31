{-# LANGUAGE OverloadedStrings #-}

module CLI.User.Auth
  ( registerScreen
  , loginScreen
  ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Time (fromGregorian)
import App.Env (AppM)
import App.Error (AppError(..))
import Models.User (User(..))
import Services.Auth (registerUser, loginUser)
import CLI.Display (printHeader, printSuccess, printError)
import CLI.Prompt (ask, askHidden, selectFrom)

registerScreen :: AppM ()
registerScreen = do
  liftIO $ printHeader "Register"
  email        <- liftIO $ ask "Email"
  password     <- liftIO $ askHidden "Password"
  firstName    <- liftIO $ ask "First Name"
  lastName     <- liftIO $ ask "Last Name"
  occupation   <- liftIO $ ask "Occupation"
  organization <- liftIO $ ask "Organization"
  userTypeChoice <- liftIO $ selectFrom "Register as:" ["User", "Admin"]
  let uType = case userTypeChoice of
                Just 2 -> "admin"
                _      -> "user"
  -- Simple fixed birth date for now, can be improved later
  let birthDate = fromGregorian 2000 1 1
  result <- registerUser email password firstName lastName
              birthDate occupation organization uType
  case result of
    Left (AlreadyExists msg) -> liftIO $ printError (T.pack msg)
    Left (AuthError msg)     -> liftIO $ printError (T.pack msg)
    Left _                   -> liftIO $ printError "Registration failed"
    Right _                  -> liftIO $ printSuccess "Registration successful!"

loginScreen :: AppM (Maybe User)
loginScreen = do
  liftIO $ printHeader "Login"
  email    <- liftIO $ ask "Email"
  password <- liftIO $ askHidden "Password"
  result   <- loginUser email password
  case result of
    Left (AuthError msg) -> do
      liftIO $ printError (T.pack msg)
      return Nothing
    Left _ -> do
      liftIO $ printError "Login failed"
      return Nothing
    Right user -> do
      liftIO $ printSuccess $ "Welcome, " <> userFirstName user <> "!"
      return (Just user)