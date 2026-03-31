module Main where

import Control.Monad.IO.Class (liftIO)
import DB.Connection (withConnection)
import DB.Migration (runMigrations)
import App.Env (Env(..), AppM, runAppM)
import CLI.Repl (startRepl)
import Control.Concurrent.Chan (newChan)
import Services.Notification (startNotificationWorker)

main :: IO ()
main = withConnection "opac.db" $ \conn -> do
  runMigrations conn
  chan <- newChan
  let env = Env
        { envConnection = conn
        , envDBPath     = "opac.db"
        , envReturnChan = chan
        }
  startNotificationWorker env
  runAppM env startRepl