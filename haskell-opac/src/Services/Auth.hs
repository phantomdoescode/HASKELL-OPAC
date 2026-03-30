{-# LANGUAGE OverloadedStrings #-}

module Services.Auth
  ( registerUser
  , loginUser
  , hashPassword
  , verifyPassword
  ) where

import Control.Monad.Reader (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (Day)
import Crypto.BCrypt
  ( hashPasswordUsingPolicy
  , slowerBcryptHashingPolicy
  , validatePassword
  )
import App.Env (AppM)
import App.Error (AppError(..))
import Models.User (User(..))
import Queries.User (insertUser, getUserByEmail)

-- Hash a plaintext password
hashPassword :: Text -> IO (Either AppError Text)
hashPassword plaintext = do
  mHash <- hashPasswordUsingPolicy slowerBcryptHashingPolicy
              (TE.encodeUtf8 plaintext)
  case mHash of
    Nothing   -> return $ Left (AuthError "Failed to hash password")
    Just hash -> return $ Right (TE.decodeUtf8 hash)

-- Verify a plaintext password against a stored hash
verifyPassword :: Text -> Text -> Bool
verifyPassword plaintext hash =
  validatePassword
    (TE.encodeUtf8 hash)
    (TE.encodeUtf8 plaintext)

-- Register a new user
registerUser
  :: Text  -- email
  -> Text  -- plaintext password
  -> Text  -- first name
  -> Text  -- last name
  -> Day   -- birth date
  -> Text  -- occupation
  -> Text  -- organization
  -> AppM (Either AppError User)
registerUser email password firstName lastName birthDate occupation organization = do
  -- Check if email already exists
  existing <- getUserByEmail email
  case existing of
    Just _  -> return $ Left (AlreadyExists "Email is already registered")
    Nothing -> do
      result <- liftIO $ hashPassword password
      case result of
        Left err   -> return $ Left err
        Right hash -> do
          let newUser = User
                { userID           = 0  -- DB assigns the real ID
                , userPasswordHash = hash
                , userFirstName    = firstName
                , userLastName     = lastName
                , userEmail        = email
                , userBirthDate    = birthDate
                , userOccupation   = occupation
                , userOrganization = organization
                }
          insertUser newUser
          return $ Right newUser

-- Login a user
loginUser :: Text -> Text -> AppM (Either AppError User)
loginUser email password = do
  mUser <- getUserByEmail email
  case mUser of
    Nothing   -> return $ Left (AuthError "Email not found")
    Just user ->
      if verifyPassword password (userPasswordHash user)
        then return $ Right user
        else return $ Left (AuthError "Incorrect password")