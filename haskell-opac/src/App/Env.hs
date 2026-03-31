module App.Env
  ( Env(..)
  , AppM
  , runAppM
  ) where

import Control.Concurrent.Chan (Chan)
import Control.Monad.Reader (ReaderT, runReaderT)
import Database.SQLite.Simple (Connection)

-- The shared environment passed through the whole app
data Env = Env
  { envConnection :: Connection
  , envDBPath     :: FilePath
  , envReturnChan :: Chan Int
  }

-- Every function in the app runs inside AppM
type AppM = ReaderT Env IO

-- Run an AppM action given an environment
runAppM :: Env -> AppM a -> IO a
runAppM = flip runReaderT